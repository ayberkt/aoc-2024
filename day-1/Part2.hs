module Part2 where

import Data.List
import System.IO

import Part1 hiding (main)

alpha :: Int -> [Int] -> Int
alpha m ns = m * length (filter (== m) ns)

result :: [(Int, Int)] -> Int
result ps = sum $ flip alpha ys <$> (fst <$> ps)
  where
    ys :: [Int]
    ys = snd <$> ps

main :: IO ()
main = do
  handle  <- openFile "day-1/input.txt" ReadMode
  content <- hGetContents handle
  print $ result (parse content)
  hClose handle
