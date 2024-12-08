module Part2 where

import Data.List (nub)
import System.IO (hClose, hGetContents, openFile, IOMode(ReadMode))

import Part1 (Antenna, groupAntennae, allAntennaeinRow, allAntennae)
import Utils (csub, scale)

antinodesWRT :: Int -> Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinodesWRT ht wd v u = beta <$> range
  where
    withinGrid :: (Int, Int) -> Bool
    withinGrid (i, j) = i >= 0 && i < ht && j >= 0 && j < wd

    beta :: Int -> (Int, Int)
    beta k = v `csub` (scale k $ v `csub` u)

    range :: [Int]
    range = takeWhile (withinGrid . beta) [1..]

numAntinodes :: Int -> Int -> [Antenna] -> [(Int, Int)]
numAntinodes ht wd as =
  concat [ concat $ antinodesWRT ht wd v <$> filter (/= v) vs | v <- vs ]
    where
      vs :: [(Int, Int)]
      vs = snd <$> as

main :: IO ()
main = do
  handle  <- openFile "day-8/input.txt" ReadMode
  content <- hGetContents handle

  let
    ls = lines content
    h  = length ls
    w  = length $ head ls

  let antennaeGrouped = groupAntennae (allAntennae ls) :: [[Antenna]]

  print . length . nub . concat $ numAntinodes h w <$> antennaeGrouped

  hClose handle
