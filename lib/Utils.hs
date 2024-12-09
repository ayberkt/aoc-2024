module Utils where

import Data.Char (isDigit)
import Data.List (sortOn, groupBy)
import GHC.Num (integerBit)

parseInt :: String -> Int
parseInt = read

parseDigit :: Char -> Int
parseDigit = parseInt . return

integer :: Int -> Integer
integer n = fromIntegral n

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


cadd :: (Int, Int) -> (Int, Int) -> (Int, Int)
cadd (i1, j1) (i2, j2) = (i1+i2, j1+j2)

cneg :: (Int, Int) -> (Int, Int)
cneg (i, j) = (-i, -j)

csub :: (Int, Int) -> (Int, Int) -> (Int, Int)
csub u v = u `cadd` cneg v

scale :: Int -> (Int, Int) -> (Int, Int)
scale k (i, j) = (k*i, k*j)

numSatisfying :: (a -> Bool) -> [a] -> Int
numSatisfying p []                 = 0
numSatisfying p (x:xs) | p x       = numSatisfying p xs + 1
                       | otherwise = numSatisfying p xs


groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)

quotient :: (Ord b, Eq b) => (a -> b) -> [a] -> [[a]]
quotient f = groupOn f . sortOn f

interweave :: [a] -> [a] -> [a]
interweave []     ys     = ys
interweave xs     []     = xs
interweave (x:xs) (y:ys) = x : y : interweave xs ys

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 y (x:xs) = y:xs
insertAt n y (x:xs) = x : insertAt (n-1) y xs
