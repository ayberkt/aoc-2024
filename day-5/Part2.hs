module Part2 where

import Data.Char   (isSpace)
import Data.List   (sortBy)
import Data.Maybe  ()
import Data.String ()
import System.IO   (hClose, hGetContents, openFile, IOMode(ReadMode))

import qualified Part1

solution :: [String] -> [String] -> Int
solution s t = sum $ Part1.middle <$> sortBy compare <$> pageLists
  where
   rules :: [(Int, Int)]
   rules = Part1.parseRule <$> s

   below :: Int -> Int -> Bool
   below m n = (m, n) `elem` rules

   compare :: Int -> Int -> Ordering
   compare m n = if m `below` n then LT else GT

   pageLists :: [[Int]]
   pageLists = filter (not . Part1.isSorted below) $ Part1.parsePageList <$> t

main :: IO ()
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let rules = takeWhile (/= "") $ lines content
  let pages = tail . dropWhile (/= "") $ lines content
  print $ solution rules pages
  hClose handle
