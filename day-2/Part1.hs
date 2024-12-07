module Part1 where

import Data.List
import Data.String
import System.IO

isSafe :: [Int] -> Bool
isSafe ns = (cond1 || cond2) && all (\x -> x >= 1 && x <= 3) (abs <$> ds)
  where
    ds :: [Int]
    ds = [ (n - m) | (m, n) <- zip ns (tail ns)]

    cond1 :: Bool
    cond1 = all (> 0) ds

    cond2 :: Bool
    cond2 = all (< 0) ds

solution :: [String] -> Int
solution ls = length $ filter isSafe reports
  where
    f :: String -> [Int]
    f = (read <$>) . words

    reports :: [[Int]]
    reports = f <$> ls

main :: IO ()
main = do
  handle  <- openFile "day-2/input.txt" ReadMode
  content <- hGetContents handle
  let result = solution $ lines content
  print result
  hClose handle
