module Part1 where

import Data.List
import Data.String
import System.IO
import Data.Char (isSpace)
import Data.Maybe

allCoordinates :: Int -> Int -> [(Int, Int)]
allCoordinates w h = ((,) <$> [0..h-1]) <*> [0..w-1]

walkEast      :: Int -> Int -> [(Int, Int)]
walkSouth     :: Int -> Int -> [(Int, Int)]
walkSouthEast :: Int -> Int -> [(Int, Int)]
walkSouthWest :: Int -> Int -> [(Int, Int)]

walkEast      i j = [ (i,   j+k) | k <- [0..3] ]
walkSouth     i j = [ (i+k, j)   | k <- [0..3] ]
walkSouthEast i j = [ (i+k, j+k) | k <- [0..3] ]
walkSouthWest i j = [ (i+k, j-k) | k <- [0..3] ]

walks :: [Int -> Int -> [(Int, Int)]]
walks = [walkEast, walkSouth, walkSouthEast, walkSouthWest]

numXMAS :: [[Char]] -> (Int, Int) -> Int
numXMAS xss (i, j) = length $ filter (\s -> s == "XMAS" || s == "SAMX") allWalks
  where
    width :: Int
    width = length (head xss)

    height :: Int
    height = length xss

    index :: Int -> Int -> Maybe Char
    index i j = if i >= 0 && i < height && j >= 0 && j <  width then
                  Just $ xss !! i !! j
                else
                  Nothing

    allWalks :: [String]
    allWalks = [ catMaybes [ index i j  |  (i, j) <- beta i j ] | beta <- walks ]

solution :: [String] -> Int
solution xss = sum $ numXMAS xss <$> coords
  where
    width :: Int
    width = length (head xss)

    height :: Int
    height = length xss

    coords :: [(Int, Int)]
    coords = allCoordinates width height

main :: IO ()
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let result = solution $ lines content
  print result
  hClose handle
