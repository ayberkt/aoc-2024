module Part1 where

import Data.List
import Data.String
import System.IO
import Data.Char

import Utils

antinodeWRT :: (Int, Int) -> (Int, Int) -> (Int, Int)
antinodeWRT v u = v `csub` (scale 2 $ v `csub` u)


type Antenna = (Char, (Int, Int))

allAntennaeinRow :: Int -> [Char] -> [Antenna]
allAntennaeinRow i row = [ (c, (i, j)) | (c, j) <- zip row [0..l-1], c /= '.' ]
  where
    l = length row

allAntennae :: [[Char]] -> [Antenna]
allAntennae grid =
  concat [ allAntennaeinRow i row | (row, i) <- zip grid [0..l-1]]
    where
      l = length grid

groupAntennae :: [Antenna] -> [[Antenna]]
groupAntennae = quotient fst

numAntinodes :: Int -> Int -> [Antenna] -> [(Int, Int)]
numAntinodes h w as = filter withinGrid $ possibleAntinodes
  where
    vs :: [(Int, Int)]
    vs = snd <$> as

    withinGrid :: (Int, Int) -> Bool
    withinGrid (i, j) = i >= 0 && i < h && j >= 0 && j < w

    possibleAntinodes :: [(Int, Int)]
    possibleAntinodes = concat [ antinodeWRT v <$> (filter (/= v) vs) | v <- vs ]

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

  -- print result
  hClose handle
