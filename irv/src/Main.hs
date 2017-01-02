module Main where

import Control.Monad.Trans.Writer.Lazy (Writer, runWriter, tell, writer)
import qualified Data.ByteString.Lazy.Char8 as C
    (ByteString, hGetContents, pack)
import Data.Csv (HasHeader(NoHeader), decode)
import Data.List (intercalate, sortBy)
import qualified Data.Text as T (Text, unpack)
import qualified Data.Vector as V
import Safe (headMay)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.IO
    (Handle, IOMode(ReadMode), hSetBinaryMode, openFile, stdin, withFile)


type Option = String
type Round = Int
type Vote = [Option]


main :: IO ()
main = do
  args <- getArgs
  if length args > 1 then showUsageAndExit (ExitFailure 1)
  else
    if headMay args == Just "-h" then showUsageAndExit ExitSuccess
    else do
      handle <- if length args == 1
                then openFile (head args) ReadMode
                else return stdin
      hSetBinaryMode handle True
      bytes <- C.hGetContents handle
      let (winner, output) = findWinner $ getVotes bytes
      putStrLn $ unlines output
      putStrLn $ show winner ++ " wins!"


showUsageAndExit :: ExitCode -> IO ()
showUsageAndExit code = do
  putStrLn "Usage: irv <votes_csv>"
  putStrLn "       cat <votes_csv> | irv"
  exitWith code

getVotes :: C.ByteString -> [Vote]
getVotes bytes =
  let dec = decode NoHeader bytes :: Either String (V.Vector [T.Text])
      unpackVotes (Left e) = error e
      unpackVotes (Right v) = map (map T.unpack) (V.toList v)
   in unpackVotes dec


findWinner :: [Vote] -> (Option, [String])
findWinner votes = runWriter $ findWinner' votes 1 [] ()


findWinner' :: [Vote] -> Round -> [Option] -> () -> Writer [String] Option
findWinner' votes rnd eliminated _ =
  let roundResult = countVotes votes eliminated
      voteLog =
          [[] | rnd > 1]
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
    | fst o < i                 = error "Out of order votes"
    | i == (-1) || fst o == i   = eliminate' os (snd o:e) (fst o)
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
