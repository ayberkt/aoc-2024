module Utils where

import Data.Char (isDigit)

parseInt :: String -> Int
parseInt = read

splitAux :: String -> (String, String)
splitAux s =
  let
    ds = takeWhile isDigit s
  in
    if null ds then
      (ds, dropWhile (not . isDigit) s)
    else
      (ds, drop (length ds)  s)

numbers :: String -> [Int]
numbers [] = []
numbers s  =  case splitAux s of
                ([], s') -> numbers s'
                (ds, s') -> parseInt ds : numbers (dropWhile isDigit s')

digitsAux :: Int -> [Int]
digitsAux 0 = []
digitsAux n = let (q, r) = divMod n 10 in r : digitsAux q

digits :: Int -> [Int]
digits = reverse . digitsAux

(<.>) :: Int -> Int -> Int
(<.>) m n = sum [ k * d |  (d, k) <- zip (digitsAux n ++ digitsAux m) tens ]
  where
    tens = (10 ^) <$> [0..]
