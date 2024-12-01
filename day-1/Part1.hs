module Part1 where

import Data.List

import System.IO

parseInts :: String -> (Int, Int)
parseInts line = let [s, t] = words line in (read s, read t)

parse :: String -> [(Int, Int)]
parse s = parseInts <$> lines s

dist :: Int -> Int -> Int
dist m n = abs (m - n)

totalDistance :: [(Int, Int)] -> Int
totalDistance ps = sum $ zipWith dist (sort xs) (sort ys)
  where
    xs :: [Int]
    xs = fst <$> ps

    ys :: [Int]
    ys = snd <$> ps

main :: IO ()
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  putStrLn $ "Total distance: " ++  show (totalDistance (parse content))
  hClose handle
