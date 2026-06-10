//! A teaching implementation of beam search.
//!
//! Beam search finds a high-scoring path through an exponentially large tree
//! of choices by exploring it level by level, keeping only the `beam_width`
//! most promising partial solutions (the "beam") at each level and discarding
//! the rest. It sits between two extremes, trading optimality for time:
//!
//! | algorithm               | time for a depth-L problem | finds the optimum?        |
//! |-------------------------|----------------------------|---------------------------|
//! | greedy search (width 1) | O(L·V)                     | no                        |
//! | beam search (width B)   | O(L·B·V·log B)             | no, but close in practice |
//! | exhaustive search       | O(V^L)                     | yes                       |
//!
//! (V is the branching factor: how many successors a state can have.)
//!
//! Beam search is the standard way to decode output sequences from machine
//! translation, speech recognition, and other sequence models, where V is a
//! vocabulary of tens of thousands of tokens and exhaustive search is
//! hopeless.
//!
//! Run `cargo run` for a demonstration on a toy language model where greedy,
//! beam, and exhaustive search each find a different sentence, and
//! `cargo test` for the test suite.

use std::cmp::{Ordering, Reverse};
use std::collections::BinaryHeap;
use std::rc::Rc;

// ---------------------------------------------------------------------------
// The search problem interface
// ---------------------------------------------------------------------------

/// A problem that beam search can solve: a (possibly enormous) graph of
/// states, each step out of a state carrying a probability.
///
/// Step scores are *log*-probabilities, for two standard reasons:
///
/// * multiplying thousands of probabilities underflows `f64`, while adding
///   their logs is numerically safe; and
/// * because probabilities are at most 1, every log-probability is <= 0, so a
///   path's score can only fall as the path grows. [`beam_search`] relies on
///   this to know when it can stop early.
pub trait SearchProblem {
    type State: Clone;

    /// The state every path starts from.
    fn initial_state(&self) -> Self::State;

    /// Is this state a valid place for a path to end?
    fn is_final(&self, state: &Self::State) -> bool;

    /// Every state reachable from `state` in one step, paired with the
    /// log-probability (<= 0) of taking that step.
    fn successors(&self, state: &Self::State) -> Vec<(Self::State, f64)>;
}

// ---------------------------------------------------------------------------
// Hypotheses: partial solutions, stored as a shared tree
// ---------------------------------------------------------------------------

/// One node in the tree of states explored so far.
///
/// A hypothesis is a path from the root of this tree to some node, so storing
/// a pointer to its *last* node is enough: the `parent` links recover the
/// rest. Extending a hypothesis is O(1) — allocate one node — and hypotheses
/// that share a prefix share its nodes:
///
/// ```text
///              <s>
///             /    \
///          the      a        the beam {"the cat", "the dog", "a dog"}
///         /   \      \       is just three pointers to the leaves
///      cat     dog    dog
/// ```
///
/// `Rc` (a reference-counted pointer) makes the sharing safe: a node is freed
/// as soon as no surviving hypothesis runs through it. Production decoders use
/// the same idea, usually phrased as per-step "backpointer" tables. The
/// obvious alternative — each hypothesis owning a `Vec` of its states — would
/// copy a length-t path for every candidate at step t, adding a factor of L
/// to the whole search.
struct PathNode<S> {
    state: S,
    parent: Option<Rc<PathNode<S>>>,
}

/// A partial solution: a path through the state tree plus its score so far.
struct Hypothesis<S> {
    /// The path, represented by its last node (see [`PathNode`]).
    last: Rc<PathNode<S>>,
    /// Sum of the log-probabilities of every step taken so far — that is,
    /// the log of the probability of the whole path.
    score: f64,
}

impl<S> Hypothesis<S> {
    /// A path containing only `state`, with probability 1 (log 1 = 0).
    fn start(state: S) -> Self {
        let last = Rc::new(PathNode {
            state,
            parent: None,
        });
        Hypothesis { last, score: 0.0 }
    }

    fn last_state(&self) -> &S {
        &self.last.state
    }

    /// A new hypothesis: this one plus one more step. O(1).
    fn extended(&self, state: S, log_prob: f64) -> Self {
        let parent = Some(Rc::clone(&self.last));
        Hypothesis {
            last: Rc::new(PathNode { state, parent }),
            score: self.score + log_prob,
        }
    }

    /// The full path, root first, rebuilt by walking the parent links. O(L) —
    /// affordable because it is called once, on the winner, not during search.
    fn path(&self) -> Vec<S>
    where
        S: Clone,
    {
        let mut states = Vec::new();
        let mut node = Some(&self.last);
        while let Some(n) = node {
            states.push(n.state.clone());
            node = n.parent.as_ref();
        }
        states.reverse();
        states
    }
}

/// Hypotheses are ordered by score alone — all the search ever asks is
/// "which of these is more probable?". `f64::total_cmp` supplies the total
/// order that `Ord` demands (plain `<` on floats is only a partial order,
/// because of NaN).
impl<S> Ord for Hypothesis<S> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.score.total_cmp(&other.score)
    }
}

impl<S> PartialOrd for Hypothesis<S> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<S> PartialEq for Hypothesis<S> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl<S> Eq for Hypothesis<S> {}

// ---------------------------------------------------------------------------
// Top-K selection
// ---------------------------------------------------------------------------

/// Keeps the K highest-scoring hypotheses offered to it, in O(log K) each.
///
/// The trick is a *min*-heap holding at most K elements (std's `BinaryHeap`
/// is a max-heap; wrapping every element in `Reverse` flips it). The root of
/// a min-heap is the worst element of the current top K — exactly the one a
/// new candidate must beat to deserve a slot.
///
/// This is one of the two places the production big-O is won or lost: each
/// step of the search must select the best B of ~B·V candidates, and doing it
/// by sorting them all would cost O(B·V·log(B·V)) instead of O(B·V·log B).
struct TopK<S> {
    capacity: usize,
    heap: BinaryHeap<Reverse<Hypothesis<S>>>,
}

impl<S> TopK<S> {
    fn new(capacity: usize) -> Self {
        assert!(capacity > 0, "cannot keep a top 0");
        TopK {
            capacity,
            heap: BinaryHeap::with_capacity(capacity),
        }
    }

    /// Admits `candidate` if it is among the K best seen so far. O(log K).
    fn insert(&mut self, candidate: Hypothesis<S>) {
        if self.heap.len() < self.capacity {
            self.heap.push(Reverse(candidate));
        } else if candidate > self.heap.peek().expect("capacity > 0").0 {
            // Better than the worst of the current top K: replace it.
            self.heap.pop();
            self.heap.push(Reverse(candidate));
        }
        // Otherwise the candidate cannot be in the top K; drop it.
    }

    /// The kept hypotheses, best first.
    fn into_best_first(self) -> Vec<Hypothesis<S>> {
        // Ascending order of `Reverse(h)` is descending order of `h`.
        let ascending = self.heap.into_sorted_vec();
        ascending.into_iter().map(|Reverse(h)| h).collect()
    }
}

// ---------------------------------------------------------------------------
// Beam search itself
// ---------------------------------------------------------------------------

/// Searches for the most probable path from the initial state to a final
/// state, keeping at most `beam_width` partial paths alive at any time.
///
/// Returns the best complete path found and its score (a log-probability;
/// `.exp()` turns it back into a probability), or `None` if no final state
/// was reached. `max_steps` bounds the length of a path; it is what
/// guarantees termination when the state graph has cycles.
///
/// # The algorithm
///
/// Keep the B = `beam_width` best partial paths, the *beam*. At each step,
/// extend every path in the beam by every possible successor, and of those
/// ~B·V candidates keep only the best B as the next beam. Candidates that
/// reach a final state graduate out of the beam into `best_finished` instead.
/// Greedy search is the special case B = 1; as B grows the search approaches
/// (but never cheaply reaches) exhaustive search.
///
/// # Complexity
///
/// Each step creates B·V candidates in O(1) each (the shared path tree —
/// see [`PathNode`]) and offers each to a bounded min-heap in O(log B)
/// ([`TopK`]), so a step costs O(B·V·log B) and a full search of L steps
/// costs O(L·B·V·log B) time and O(L·B) space. That matches a production
/// sequence decoder, which batches the same candidate scoring onto a GPU and
/// keeps the same per-step backpointer tables. (Production decoders also add
/// refinements that change *quality* rather than complexity — length
/// normalization, sampling, deduplication — omitted here.)
pub fn beam_search<P: SearchProblem>(
    problem: &P,
    beam_width: usize,
    max_steps: usize,
) -> Option<(Vec<P::State>, f64)> {
    assert!(beam_width > 0, "beam_width must be at least 1");

    let start = Hypothesis::start(problem.initial_state());
    if problem.is_final(start.last_state()) {
        return Some((start.path(), start.score));
    }

    // The beam: the best unfinished hypotheses found so far, best first.
    let mut beam = vec![start];
    // The best hypothesis that has reached a final state, if any has.
    let mut best_finished: Option<Hypothesis<P::State>> = None;

    for _ in 0..max_steps {
        // Stop early when the answer provably cannot improve. Every step adds
        // a log-probability <= 0, so scores only fall as paths grow: once the
        // best *unfinished* hypothesis already scores no better than the best
        // *finished* one, no descendant of anything in the beam can win.
        match (&best_finished, beam.first()) {
            (_, None) => break, // every unfinished path hit a dead end
            (Some(done), Some(best_open)) if done >= best_open => break,
            _ => {}
        }

        // Extend everything in the beam every way; keep the best B results.
        let mut survivors = TopK::new(beam_width);
        for hypothesis in &beam {
            for (state, log_prob) in problem.successors(hypothesis.last_state()) {
                debug_assert!(
                    log_prob <= 0.0,
                    "step scores must be log-probabilities, got {log_prob}"
                );
                let candidate = hypothesis.extended(state, log_prob);
                if problem.is_final(candidate.last_state()) {
                    // Finished paths leave the beam; remember the best one.
                    if best_finished.as_ref().is_none_or(|b| candidate > *b) {
                        best_finished = Some(candidate);
                    }
                } else {
                    survivors.insert(candidate);
                }
            }
        }
        beam = survivors.into_best_first();
    }

    best_finished.map(|h| (h.path(), h.score))
}

/// Brute-force baseline: enumerate every complete path and keep the best.
///
/// O(V^L) time — astronomically slow on real problems, which is the whole
/// reason beam search exists, but fine on the toy model below, where it
/// serves as the answer key. Assumes the state graph is acyclic; on a cyclic
/// problem it would recurse forever.
pub fn exhaustive_best<P: SearchProblem>(problem: &P) -> Option<(Vec<P::State>, f64)> {
    fn explore<P: SearchProblem>(
        problem: &P,
        path: &mut Vec<P::State>,
        score: f64,
        best: &mut Option<(Vec<P::State>, f64)>,
    ) {
        let state = path.last().expect("path is never empty").clone();
        if problem.is_final(&state) {
            if best
                .as_ref()
                .is_none_or(|(_, best_score)| score > *best_score)
            {
                *best = Some((path.clone(), score));
            }
            return;
        }
        for (next, log_prob) in problem.successors(&state) {
            path.push(next);
            explore(problem, path, score + log_prob, best);
            path.pop();
        }
    }

    let mut best = None;
    explore(problem, &mut vec![problem.initial_state()], 0.0, &mut best);
    best
}

// ---------------------------------------------------------------------------
// Example: decoding the most probable sentence from a tiny language model
// ---------------------------------------------------------------------------

/// A toy bigram language model: the probability of each word depends only on
/// the word before it, so a search state is simply the most recent word.
/// Sentences run from the start marker `<s>` to the end marker `</s>`, and a
/// sentence's probability is the product of its step probabilities.
///
/// (A real neural decoder conditions on the whole prefix through cached model
/// state rather than on one word, but the search algorithm is identical.)
///
/// The table is rigged so that the best sentence hides behind locally bad
/// choices — the situation beam search exists for:
///
/// * Greedy search grabs `the` (p = 0.45, the safest first word) and is stuck
///   with its mediocre continuations: "the cat meows", p = 0.1485.
/// * A beam of 2 also keeps `a` (p = 0.30) alive long enough to reach
///   "a dog barks", p = 0.189.
/// * A beam of 3 also keeps `every` (p = 0.25) — the *worst*-looking first
///   word — whose near-certain continuations pay off:
///   "every great dog barks", p = 0.225, the true optimum.
pub struct ToyLanguageModel;

impl ToyLanguageModel {
    /// P(next word | current word). Each row sums to 1.
    #[rustfmt::skip]
    fn next_words(word: &str) -> &'static [(&'static str, f64)] {
        match word {
            "<s>"   => &[("the", 0.45), ("a", 0.30), ("every", 0.25)],
            "the"   => &[("cat", 0.55), ("dog", 0.45)],
            "a"     => &[("dog", 0.70), ("cat", 0.30)],
            "every" => &[("great", 1.00)],
            "great" => &[("dog", 1.00)],
            "cat"   => &[("meows", 0.60), ("sleeps", 0.40)],
            "dog"   => &[("barks", 0.90), ("sleeps", 0.10)],
            "meows" | "barks" | "sleeps" => &[("</s>", 1.00)],
            "</s>"  => &[], // the end marker has no continuations
            other   => unreachable!("unknown word {other:?}"),
        }
    }
}

impl SearchProblem for ToyLanguageModel {
    type State = &'static str;

    fn initial_state(&self) -> Self::State {
        "<s>"
    }

    fn is_final(&self, word: &Self::State) -> bool {
        *word == "</s>"
    }

    fn successors(&self, word: &Self::State) -> Vec<(Self::State, f64)> {
        Self::next_words(word)
            .iter()
            .map(|&(next, p)| (next, p.ln()))
            .collect()
    }
}

fn main() {
    let model = ToyLanguageModel;

    println!("Searching for the most probable sentence of a toy language model.\n");
    for width in 1..=3 {
        let (path, score) =
            beam_search(&model, width, 100).expect("the toy model always reaches </s>");
        report(&format!("beam width {width}"), &path, score);
    }
    let (path, score) = exhaustive_best(&model).expect("the toy model has complete sentences");
    report("exhaustive", &path, score);

    println!(
        "\nGreedy search (a beam of width 1) commits to the locally safest first\n\
         word and can never recover; wider beams keep locally worse openings\n\
         alive long enough for their better continuations to win."
    );
}

/// Prints one result, hiding the <s> / </s> markers for readability.
fn report(label: &str, path: &[&'static str], score: f64) {
    let sentence = path[1..path.len() - 1].join(" ");
    println!(
        "  {label:<13} \"{sentence}\"  (probability {:.4})",
        score.exp()
    );
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Asserts that searching the toy model with the given beam width finds
    /// exactly `expected_path` with probability `expected_prob`.
    fn assert_decodes_to(width: usize, expected_path: &[&str], expected_prob: f64) {
        let (path, score) =
            beam_search(&ToyLanguageModel, width, 100).expect("search should find a sentence");
        assert_eq!(path, expected_path);
        assert!(
            (score.exp() - expected_prob).abs() < 1e-12,
            "expected probability {expected_prob}, got {}",
            score.exp()
        );
    }

    #[test]
    fn width_one_is_greedy() {
        assert_decodes_to(
            1,
            &["<s>", "the", "cat", "meows", "</s>"],
            0.45 * 0.55 * 0.60,
        );
    }

    #[test]
    fn width_two_recovers_from_the_first_greedy_mistake() {
        assert_decodes_to(2, &["<s>", "a", "dog", "barks", "</s>"], 0.30 * 0.70 * 0.90);
    }

    #[test]
    fn width_three_finds_the_global_optimum() {
        assert_decodes_to(
            3,
            &["<s>", "every", "great", "dog", "barks", "</s>"],
            0.25 * 1.00 * 1.00 * 0.90,
        );
    }

    #[test]
    fn a_wide_enough_beam_agrees_with_exhaustive_search() {
        let (beam_path, beam_score) = beam_search(&ToyLanguageModel, 1000, 100).unwrap();
        let (best_path, best_score) = exhaustive_best(&ToyLanguageModel).unwrap();
        assert_eq!(beam_path, best_path);
        assert!((beam_score - best_score).abs() < 1e-12);
    }

    #[test]
    fn top_k_keeps_the_k_best_in_order() {
        fn hypothesis_with_score(score: f64) -> Hypothesis<()> {
            let mut h = Hypothesis::start(());
            h.score = score;
            h
        }

        let mut top = TopK::new(3);
        for score in [-2.3, -0.1, -0.7, -1.2, -0.4] {
            top.insert(hypothesis_with_score(score));
        }
        let kept: Vec<f64> = top.into_best_first().iter().map(|h| h.score).collect();
        assert_eq!(kept, vec![-0.1, -0.4, -0.7]);
    }

    #[test]
    fn a_problem_with_no_solution_returns_none() {
        struct DeadEnd;
        impl SearchProblem for DeadEnd {
            type State = u32;
            fn initial_state(&self) -> u32 {
                0
            }
            fn is_final(&self, _: &u32) -> bool {
                false
            }
            fn successors(&self, _: &u32) -> Vec<(u32, f64)> {
                Vec::new()
            }
        }
        assert_eq!(beam_search(&DeadEnd, 4, 100), None);
    }

    #[test]
    fn an_initial_state_that_is_already_final_is_the_answer() {
        struct Trivial;
        impl SearchProblem for Trivial {
            type State = &'static str;
            fn initial_state(&self) -> Self::State {
                "done"
            }
            fn is_final(&self, s: &Self::State) -> bool {
                *s == "done"
            }
            fn successors(&self, _: &Self::State) -> Vec<(Self::State, f64)> {
                Vec::new()
            }
        }
        assert_eq!(beam_search(&Trivial, 1, 10), Some((vec!["done"], 0.0)));
    }

    #[test]
    fn max_steps_caps_the_search() {
        // The shortest complete sentence takes four steps, so two are not enough.
        assert_eq!(beam_search(&ToyLanguageModel, 3, 2), None);
    }
}
