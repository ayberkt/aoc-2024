module Part2 where

import Data.Array (array)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import System.IO (hClose, hGetContents, openFile, IOMode(ReadMode))
import Control.Parallel.Strategies (parMap, rdeepseq)

import Utils (directions, east, north, south, step, west)
import Part1 (GuardState, NextStep(..), nextStep, containsGuard, isGuard, Grid,
              rotate, (!?))
import qualified Part1

type AdditionalObstruction = (Int, Int)

resultsInLoop :: Grid -> AdditionalObstruction -> GuardState -> Bool
resultsInLoop grid coordObs = playAux []
  where
    considerNextStep :: GuardState -> NextStep
    considerNextStep gs =
      case grid !? nextStep gs of
        Just '#'                           -> Obstructed
        Just '.' | nextStep gs == coordObs -> Obstructed
        Just '.'                           -> CanMove
        Nothing                            -> HitWall

    playAux :: [GuardState] -> GuardState -> Bool
    playAux visited gs@(c, d) | gs `elem` visited = True
    playAux visited gs@(c, d) =
      case considerNextStep gs of
        CanMove    -> playAux (gs:visited) (nextStep gs, d)
        Obstructed -> playAux (gs:visited) (rotate gs)
        HitWall    -> False

main :: IO ()
main = do
  handle  <- openFile "day-6/input.txt" ReadMode
  content <- hGetContents handle

  let
    ls      = lines content
    gi      = fromJust $ findIndex containsGuard ls
    gj      = fromJust $ findIndex isGuard (ls !! gi)
    d       = fromJust $ Part1.guardDirection (ls !! gi !! gj)
    h       = length ls
    w       = length $ head ls
    range   = [ (i, j) | i <- [0..h-1], j <- [0..w-1] ]
    assocs0 = [ ((i, j), ls !! i !! j) | (i, j) <- range ]
    assocs  = (\(p, x) -> if isGuard x then (p, '.') else (p, x)) <$> assocs0
    grid    = array ((0, 0), (h-1, w-1)) assocs :: Grid
    pathP1  = init . init $ Part1.play grid ((gi, gj), d) :: [(Int, Int)]

    resultsInLoop' :: AdditionalObstruction -> Bool
    resultsInLoop' = flip (resultsInLoop grid) ((gi, gj), d)

    -- Obstructions resulting in a loop.
  print . length . filter id $ parMap rdeepseq resultsInLoop' pathP1

  hClose handle
