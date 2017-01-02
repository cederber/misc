module Main where

import Control.Monad.Trans.Writer.Lazy
    (Writer, listen, pass, runWriter, tell, writer)
import Data.List (intercalate, sortBy)
import Safe (headMay)


type Option = String
type Round = Int
type Vote = [Option]


main :: IO ()
main = do
  let (winner, output) = findWinner votes
  putStrLn $ unlines output
  putStrLn ""
  putStrLn $ show winner ++ " wins!"


votes :: [Vote]
votes = [
    ["a", "b", "c"],
    ["a", "b", "c"],
    ["b", "a", "c"],
    ["b", "a", "c"],
    ["c", "a", "b"]]


findWinner :: [Vote] -> (Option, [String])
findWinner votes = runWriter $ findWinner' votes 1 [] ()


findWinner' :: [Vote] -> Round -> [Option] -> () -> Writer [String] Option
findWinner' votes rnd eliminated _ =
  let roundResult = countVotes votes eliminated
      voteLog =
          [[] | rnd > 0]
          ++ ["Round #" ++ show rnd]
          ++ showResult roundResult
  in if hasWinner roundResult
       then writer (snd $ head roundResult, voteLog)
       else
         let eliminatedThisRound = eliminate roundResult
             elimLog = ["Eliminate " ++ intercalate "," eliminatedThisRound]
             newEliminated = eliminated ++ eliminatedThisRound
         in tell (voteLog ++ elimLog)
             >>= findWinner' votes (rnd + 1) (eliminated ++ eliminatedThisRound)


showResult :: [(Int, Option)] -> [String]
showResult = map (\r -> show (fst r) ++ "\t" ++ snd r)


-- Count votes based on the highest-ranked not-yet-eliminated option.
-- Return in ordered list.
countVotes :: [Vote] -> [Option] -> [(Int, Option)]
countVotes votes eliminated =
  let firstValid v = headMay $ filter (not . (`elem` eliminated)) v
      validVotes = map firstValid votes
  in  sortBy (flip compare) $ foldl countFirstChoiceVote [] validVotes


eliminate :: [(Int, Option)] -> [Option]
eliminate roundResult = eliminate' (reverse roundResult) [] (-1)


eliminate' :: [(Int, Option)] -> [Option] -> Int -> [Option]
eliminate' [] e _ = e
eliminate' (o:os) e i
    | fst o < i               = error "Out of order votes"
    | i == (-1) || fst o == i = eliminate' os (snd o:e) (fst o)
    | otherwise                 = e


hasWinner :: [(Int, Option)] -> Bool
hasWinner [] = False
hasWinner roundResult = 2 * most > total
    where most = fst $ head roundResult
          total = sum $ map fst roundResult


countFirstChoiceVote :: [(Int, Option)] -> Maybe Option -> [(Int, Option)]
countFirstChoiceVote counts Nothing = counts
countFirstChoiceVote [] (Just o) = [(1, o)]
countFirstChoiceVote ((n,o1):os) (Just o)
    | o1 == o   = (n+1, o):os
    | otherwise = (n, o1):countFirstChoiceVote os (Just o)
