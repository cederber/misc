import Data.List (sort)


type Option = String
type Vote = [Option]


votes :: [Vote]
votes = [["a", "b", "c"]]


main = do
  winner <- findWinner votes
  putStrLn winner


headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay as = Just (head as)


findWinner :: [Vote] -> IO Option
findWinner votes = findWinner' votes []


findWinner' :: [Vote] -> [Option] -> IO Option
findWinner' votes eliminated = do
  roundResult <- countVotes votes eliminated
  if hasWinner roundResult
     then return (snd $ head roundResult)
     else findWinner' votes (eliminated ++ eliminate roundResult)


eliminate :: [(Int, String)] -> [String]
eliminate roundResult = eliminate' (reverse roundResult) [] (-1)


eliminate' :: [(Int, String)] -> [String] -> Int -> [String]
eliminate' [] e _ = e
eliminate' (o:os) e i
    | (fst o) < i               = error "Out of order votes" 
    | i == (-1) || (fst o) == i = eliminate' os (snd o:e) (fst o) 
    | otherwise                 = e


hasWinner :: [(Int, Option)] -> Bool
hasWinner [] = False
hasWinner roundResult = 2 * most > total
    where most = fst $ head roundResult
          total = sum (map fst roundResult)


-- Count votes based on the highest-ranked not-yet-eliminated option.
-- Return in ordered list.
countVotes :: [Vote] -> [Option] -> IO [(Int, String)]
countVotes votes eliminated = do
  let firstValid v = headMay $ filter (not . (`elem` eliminated)) v
      validVotes = map firstValid votes
      result = reverse . sort $ foldl countOption [] validVotes
  putStrLn (show result)
  return result


countOption :: [(Int, Option)] -> Maybe Option -> [(Int, Option)]
countOption counts Nothing = counts
countOption [] (Just o) = [(1, o)]
countOption ((n,o1):os) (Just o)
    | o1 == o   = (n+1, o):os
    | otherwise = (n, o1):countOption os (Just o)
