module Part1 where

import Data.Char   (isSpace)
import Data.List   ()
import Data.Maybe  ()
import Data.String ()
import System.IO   (hClose, hGetContents, openFile, IOMode(ReadMode))

parseRule :: String -> (Int, Int)
parseRule s = (read $ takeWhile (/= '|') s, read $ tail $ dropWhile (/= '|') s)

parsePageList :: String -> [Int]
parsePageList s = read $ "[" ++ s ++ "]"

isSorted :: (a -> a -> Bool) -> [a] -> Bool
isSorted ord []         = True
isSorted ord [x]        = True
isSorted ord (x1:x2:xs) = ord x1 x2 && isSorted ord (x2:xs)

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

solution :: [String] -> [String] -> Int
solution s t = sum $ middle <$> filter (isSorted below) pageLists
  where
    rules :: [(Int, Int)]
    rules = parseRule <$> s

    below :: Int -> Int -> Bool
    below m n = (m, n) `elem` rules

    allNumbers :: [Int]
    allNumbers = (fst <$> rules) ++ (snd <$> rules)

    pageLists :: [[Int]]
    pageLists = parsePageList <$> t

main :: IO ()
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let rules = takeWhile (/= "") $ lines content
  let pages = tail . dropWhile (/= "") $ lines content
  print $ solution rules pages
  hClose handle
