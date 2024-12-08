module Part1 where

import Data.List
import Data.String
import System.IO
import Data.Char

import Utils

solution :: [String] -> Int
solution ls = undefined

main :: IO ()
main = do
  handle  <- openFile "day-8/test.txt" ReadMode
  content <- hGetContents handle
  let result = solution $ lines content
  print result
  hClose handle
