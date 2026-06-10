# beam-search

A teaching implementation of beam search in Rust, using only the standard
library. Everything lives in one commented file: [`src/main.rs`](src/main.rs).

```
cargo run    # demo: greedy vs. beam vs. exhaustive search on a toy language model
cargo test   # the test suite
```

Sample output:

```
Searching for the most probable sentence of a toy language model.

  beam width 1  "the cat meows"  (probability 0.1485)
  beam width 2  "a dog barks"  (probability 0.1890)
  beam width 3  "every great dog barks"  (probability 0.2250)
  exhaustive    "every great dog barks"  (probability 0.2250)
```

The toy model is rigged so that each wider beam finds a strictly better
sentence: the most probable sentence starts with the *least* probable first
word, which greedy search discards immediately.

Although it is written for clarity, not production use, the implementation
has the same asymptotic complexity as a serious decoder — O(L·B·V·log B) for
beam width B, branching factor V, and solution length L — thanks to the two
tricks real decoders use:

1. hypotheses are pointers into a shared prefix tree ("backpointers"), so
   extending one is O(1) instead of copying the whole path; and
2. each step selects the best B of ~B·V candidates with a size-B min-heap
   (O(log B) per candidate) instead of sorting them all.
