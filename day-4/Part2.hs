module Part2 where

import Data.List
import Data.String
import System.IO
import Data.Char (isSpace)
import Data.Maybe

import qualified Part1

-- Northeast character and southwest character.
diag1 :: Int -> Int -> ((Int, Int), (Int, Int))
diag1 i j = ((i-1, j+1), (i+1, j-1))

-- Northwest character and southeast character.
diag2 :: Int -> Int -> ((Int, Int), (Int, Int))
diag2 i j = ((i-1, j-1), (i+1, j+1))

-- Expresses that the pair is exactly 'S' and 'M', ignoring the order.
delta :: Char -> Char -> Bool
delta c1 c2 = (c1 == 'S' && c2 == 'M') || (c1 == 'M' && c2 == 'S')

isXMAS :: [[Char]] -> (Int, Int) -> Bool
isXMAS grid (i, j) = fromMaybe False result
  where
    ((a,  b),  (c,  d))  = diag1 i j
    ((a', b'), (c', d')) = diag2 i j

    width :: Int
    width = length (head grid)

    height :: Int
    height = length grid

    index :: Int -> Int -> Maybe Char
    index i j = if i >= 0 && i < height && j >= 0 && j <  width then
                  Just $ grid !! i !! j
                else
                  Nothing

    result :: Maybe Bool
    result = do l1 <- index a b
                l2 <- index c d
                l3 <- index a' b'
                l4 <- index c' d'
                let r = delta l1 l2 && delta l3 l4
                l <- index i j
                return $ r && (l == 'A')

solution :: [String] -> Int
solution grid = length $ filter beta coords
  where
    beta = isXMAS grid

    width :: Int
    width = length (head grid)

    height :: Int
    height = length grid

    coords :: [(Int, Int)]
    coords = Part1.allCoordinates width height

main :: IO ()
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let result = solution $ lines content
  print result
  hClose handle
