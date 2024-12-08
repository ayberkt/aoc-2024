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

newtype Direction = Direction (Int, Int) deriving (Eq, Show)

north :: Direction
north = Direction (-1, 0)

east :: Direction
east = Direction (0, 1)

south :: Direction
south = Direction (1, 0)

west :: Direction
west = Direction (0, -1)

directions :: [Direction]
directions = [north, east, south, west]

step :: Direction -> (Int, Int) -> (Int, Int)
step (Direction (dx, dy)) (i, j) = (i+dx, j+dy)
