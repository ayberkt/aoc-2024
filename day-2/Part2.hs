module Part2 where

import Data.List
import Data.String
import System.IO

import qualified Part1

deleteAt :: Int -> [a] -> [a]
deleteAt i ns = let (xs, ys) = splitAt (i+1) ns in init xs ++ ys

isSafe :: [Int] -> Bool
isSafe ns = any Part1.isSafe [ deleteAt i ns | i <- [0..length ns-1] ]

solution :: [String] -> Int
solution ls = length $ filter isSafe reports
  where
    f :: String -> [Int]
    f = (read <$>) . words

    reports :: [[Int]]
    reports = f <$> ls

main :: IO ()
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let result = solution $ lines content
  print result
  hClose handle
