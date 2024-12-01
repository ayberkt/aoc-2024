module Part1 where

import Data.List
import Data.String
import System.IO

solution :: [String] -> Int
solution ls = undefined

main :: IO ()
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let result = solution $ lines content
  print result
  hClose handle
