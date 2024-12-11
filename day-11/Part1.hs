module Part1 where

import System.IO (IOMode(ReadMode), openFile, hGetContents, hClose)
import Utils (digits, digitsToInt, splitInHalf, numbers)

blink :: [Int] -> [Int]
blink []                                = []
blink (0:ns)                            = 1 : blink ns
blink (n:ns) | even (length (digits n)) = digitsToInt xs : digitsToInt ys : blink ns
                                            where
                                              (xs, ys) = splitInHalf (digits n)
blink (n:ns)                            = 2024 * n : blink ns

solution :: [String] -> Int
solution = length . last . take 26 . iterate blink . numbers . head

run :: IO ()
run = do
  handle  <- openFile "day-11/input.txt" ReadMode
  content <- hGetContents handle
  print . solution $ lines content
  hClose handle
