module Part1 where

import Data.List

import System.IO

parseInts :: String -> (Int, Int)
parseInts line = let [s, t] = words line in (read s, read t)

parse :: String -> [(Int, Int)]
parse s = parseInts <$> lines s

foo :: Int -> [Int] -> Int
foo m ns = m * length (filter (== m) ns)

result :: [(Int, Int)] -> Int
result ps = sum $ flip foo ys <$> (fst <$> ps)
  where
    ys :: [Int]
    ys = snd <$> ps

main :: IO ()
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  print $ result (parse content)
  hClose handle
