module Part2 where

import Data.List
import Data.String
import System.IO
import Data.Char

import Utils (numbers, (<.>))

possibilities :: Int -> [Int] -> [Int]
possibilities r [a]    = [a]
possibilities r (a:as) =
  [ a + p | p <- ps ] ++ [ a * p | p <- ps ] ++ [ p <.> a | p <- ps, p <.> a <= r ]
    where
      ps = possibilities r as

isValid :: Int -> [Int] -> Bool
isValid r ns = r `elem` possibilities r (reverse ns)

solution :: [String] -> Int
solution ls = sum [ r | (r, as) <- zip rs ass, isValid r as ]
  where
    nss :: [[Int]]
    nss = numbers <$> ls

    ass :: [[Int]]
    ass = tail <$> nss

    rs :: [Int]
    rs = head <$> nss

main :: IO ()
main = do
  handle  <- openFile "day-7/input.txt" ReadMode
  content <- hGetContents handle
  let result = solution $ lines content
  print result
  hClose handle
