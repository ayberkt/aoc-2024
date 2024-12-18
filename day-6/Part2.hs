module Part2 where

import Data.Array (array)
import Data.List (findIndex)
import Prelude hiding (lookup)
import Data.Maybe (fromJust, isJust)
import System.IO (hClose, hGetContents, openFile, IOMode(ReadMode))
import Control.Parallel.Strategies (parMap, rseq)
import qualified Data.Set as Set

import Utils (directions, east, north, south, step, west)
import Part1 (GuardState, NextStep(..), nextStep, containsGuard, isGuard, Grid,
              rotate, (!?))
import qualified Part1

type History = Set.Set GuardState

type AdditionalObstruction = (Int, Int)

resultsInLoop :: Grid -> AdditionalObstruction -> GuardState -> Bool
resultsInLoop grid coordObs = playAux Set.empty
  where
    considerNextStep :: GuardState -> NextStep
    considerNextStep gs =
      case grid !? nextStep gs of
        Just '#'                           -> Obstructed
        Just '.' | nextStep gs == coordObs -> Obstructed
        Just '.'                           -> CanMove
        Nothing                            -> HitWall

    playAux :: History -> GuardState -> Bool
    playAux visited gs@(c, d) | Set.member gs visited = True
    playAux visited gs@(c, d) =
      case considerNextStep gs of
        CanMove    -> playAux (Set.insert gs visited) (nextStep gs, d)
        Obstructed -> playAux (Set.insert gs visited) (rotate gs)
        HitWall    -> False


solution :: [String] -> IO Int
solution ls = return . length . filter id $ parMap rseq resultsInLoop' pathP1
  where
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

main :: IO ()
main = do
  handle  <- openFile "day-6/input.txt" ReadMode
  content <- hGetContents handle
  result  <- solution $ lines content
  print result
  hClose handle
