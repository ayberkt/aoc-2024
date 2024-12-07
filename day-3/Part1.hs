module Part1 where

import Data.List
import Data.String
import System.IO
import Text.Regex.TDFA
import Data.Char (isSpace)

mulRegex :: String
mulRegex = "mul\\([0-9]+,[0-9]+\\)"

getMuls :: String -> [String]
getMuls s = getAllTextMatches (s =~ mulRegex)

split :: String -> (String, String)
split s = (s1, s2)
  where
    s1 = takeWhile (/= ',') s
    s2 = tail $ dropWhile (/= ',') s

calculate :: String -> Int
calculate ('m':'u':'l':s) =
  let
    (s1, s2) = split . init . tail $ s
    (n1, n2) = (read s1, read s2) :: (Int, Int)
  in
    n1 * n2

solution :: String -> Int
solution ls = sum $ calculate <$> mulss
  where
    mulss :: [String]
    mulss = getMuls ls

main :: IO ()
main = do
  handle  <- openFile "day-3/input.txt" ReadMode
  content <- hGetContents handle
  let result = solution $ content
  print result
  hClose handle
