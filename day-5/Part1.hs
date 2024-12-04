module Part1 where

import Data.List
import Data.String
import System.IO
import Data.Char (isSpace)
import Data.Maybe

solution :: [String] -> Int
solution ss = undefined

main :: IO ()
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let result = solution $ lines content
  print result
  hClose handle
