module Irv where


import Data.List (sort)
import Safe (headMay)


type Option = String
type Vote = [Option]


findWinner :: [Vote] -> Option
findWinner votes = findWinner' votes []


findWinner' :: [Vote] -> [Option] -> Option
findWinner' votes eliminated =
  let roundResult = countVotes votes eliminated
  in if hasWinner roundResult
        then snd $ head roundResult
        else findWinner' votes ((snd $ last roundResult):eliminated)


hasWinner :: [(Int, Option)] -> Bool
hasWinner [] = False
hasWinner roundResult = 2 * most > total
    where most = fst $ head roundResult
          total = sum (map fst roundResult)


-- Count votes based on the highest-ranked not-yet-eliminated option.
-- Return in ordered list.
countVotes :: [Vote] -> [Option] -> [(Int, String)]
countVotes votes eliminated =
  let firstValid v = headMay $ filter (not . (`elem` eliminated)) v
      validVotes = map firstValid votes
  in reverse . sort $ foldl countOption [] validVotes


countOption :: [(Int, Option)] -> Maybe Option -> [(Int, Option)]
countOption counts Nothing = counts
countOption [] (Just o) = [(1, o)]
countOption ((n,o1):os) (Just o)
    | o1 == o   = (n+1, o):os
    | otherwise = (n, o1):countOption os (Just o)
