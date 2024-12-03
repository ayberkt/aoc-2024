module Part2 where

import Data.List
import Data.String
import System.IO
import Text.Regex.TDFA
import Data.Char (isSpace)

import qualified Part1

mulRegex :: String
mulRegex = "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)"

getInstructions :: String -> [String]
getInstructions s = getAllTextMatches (s =~ mulRegex)

evaluate :: [String] -> Bool -> [String]
evaluate []     _                  = []
evaluate (i:is) c | i == "do()"    = evaluate is True
                  | i == "don't()" = evaluate is False
                  | otherwise      = if c then i : evaluate is c else evaluate is c

solution :: String -> Int
solution ls = sum $ Part1.calculate <$> mulss
  where
    mulss :: [String]
    mulss = evaluate (getInstructions ls) True

main :: IO ()
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let result = solution $ content
  print result
  hClose handle
