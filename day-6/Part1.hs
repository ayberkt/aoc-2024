module Part1 where

import Data.Array.IArray (Array, (!), array, IArray(bounds))
import Data.Char ()
import Data.List (elemIndex, findIndex, nub)
import Data.Maybe (fromJust)
import Data.String ()
import System.IO (hClose, hGetContents, openFile, IOMode(ReadMode))
import Utils (directions, east, north, south, step, west)

type Grid = Array (Int, Int) Char

type GuardState = ((Int, Int), Int)

data NextStep = CanMove
              | Obstructed
              | HitWall
              deriving (Eq, Show)

isGuard :: Char -> Bool
isGuard c = c `elem` "^>v<"

containsGuard :: String -> Bool
containsGuard = any isGuard

guardDirection :: Char -> Maybe Int
guardDirection c = elemIndex c "^>v<"

infixl 9 !?

(!?) :: Grid -> (Int, Int) -> Maybe Char
(!?) grid (i, j) =
  let
    (_, (bi, bj)) = bounds grid
  in
    if i >= 0 && i <= bi && j >= 0 && j <= bj then do
      Just $ grid ! (i, j)
    else
      Nothing

nextStep :: GuardState -> (Int, Int)
nextStep (c, d) = step (directions !! d) c

rotate :: GuardState -> GuardState
rotate (c, d) = (c, (d + 1) `mod` 4)

play :: Grid -> GuardState -> [(Int, Int)]
play grid = nub . playAux []
  where
    playAux :: [(Int, Int)] -> GuardState -> [(Int, Int)]
    playAux coords gs@(c, d) =
      case considerNextStep gs of
        CanMove    -> playAux (c:coords) (nextStep gs, d)
        Obstructed -> playAux (c:coords) (rotate gs)
        HitWall    -> c:coords
      where
        considerNextStep :: GuardState -> NextStep
        considerNextStep gs = case grid !? nextStep gs of
                                Just '.' -> CanMove
                                Just '#' -> Obstructed
                                Nothing  -> HitWall

main :: IO ()
main = do
  handle  <- openFile "day-6/input.txt" ReadMode
  content <- hGetContents handle

  let
    ls      = lines content
    gi      = fromJust $ findIndex containsGuard ls
    gj      = fromJust $ findIndex isGuard (ls !! gi)
    d       = fromJust $ guardDirection (ls !! gi !! gj)
    h       = length ls
    w       = length $ head ls
    range   = [ (i, j) | i <- [0..h-1], j <- [0..w-1] ]
    assocs0 = [ ((i, j), ls !! i !! j) | (i, j) <- range ]
    assocs  = (\(p, x) -> if isGuard x then (p, '.') else (p, x)) <$> assocs0
    grid    = array ((0, 0), (h-1, w-1)) assocs :: Grid

  print . length $ play grid ((gi, gj), d)
  hClose handle
