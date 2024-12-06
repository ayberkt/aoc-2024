module Part2 where

import Data.Array.IO
import Data.Array

import Data.Char
import Data.List
import Data.Maybe
import Data.String
import System.IO   (hClose, hGetContents, openFile, IOMode(ReadMode))

import Part1 hiding (solution, step, stepNorthCase, stepEastCase, stepSouthCase, stepWestCase, play, main, printGrid)

data Orientation = North | East | South | West deriving (Eq, Show)

type GuardState = (Orientation, (Int, Int))

type PathData = [GuardState]

stepNorthCase :: Int -> Int -> (Int, Int) -> Grid -> IO (Maybe GuardState)
stepNorthCase h w (i, j) grid = do
  let nc = northCell (i, j)
  result <- Part1.indexGrid h w nc grid
  case result of
    Just '.' -> do { update nc '^' grid; update (i, j) 'X' grid; return $ Just (North, nc) }
    Just 'X' -> do { update nc '^' grid; update (i, j) 'X' grid; return $ Just (North, nc) }
    Just '#' -> do { update (i, j) '>' grid; return $ Just (East, (i, j)) }
    Nothing  -> return Nothing
    _        -> undefined

stepEastCase :: Int -> Int -> (Int, Int) -> Grid -> IO (Maybe GuardState)
stepEastCase h w (i, j) grid = do
  let nc = eastCell (i, j)
  result <- indexGrid h w nc grid
  case result of
    Just '.' -> do { update nc '>' grid; update (i, j) 'X' grid; return $ Just (East, nc) }
    Just 'X' -> do { update nc '>' grid; update (i, j) 'X' grid; return $ Just (East, nc) }
    Just '#' -> do { update (i, j) 'v' grid; return $ Just (South, (i, j)) }
    Nothing  -> return Nothing
    _        -> undefined

stepSouthCase :: Int -> Int -> (Int, Int) -> Grid -> IO (Maybe GuardState)
stepSouthCase h w (i, j) grid = do
  let nc = southCell (i, j)
  result <- indexGrid h w nc grid
  case result of
    Just '.' -> do { update nc 'v' grid; update (i, j) 'X' grid; return (Just (South, nc)) }
    Just 'X' -> do { update nc 'v' grid; update (i, j) 'X' grid; return (Just (South, nc)) }
    Just '#' -> do { update (i, j) '<' grid; return (Just (West, (i, j))) }
    Nothing  -> return Nothing
    Just c        -> error [c]

stepWestCase :: Int -> Int -> (Int, Int) -> Grid -> IO (Maybe GuardState)
stepWestCase h w (i, j) grid = do
  let nc = westCell (i, j)
  result <- indexGrid h w nc grid
  case result of
    Just '.' -> do { update nc '<' grid; update (i, j) 'X' grid; return (Just (West, nc)) }
    Just 'X' -> do { update nc '<' grid; update (i, j) 'X' grid; return (Just (West, nc)) }
    Just '#' -> do { update (i, j) '^' grid; return $ Just (North, (i, j)) }
    Nothing  -> return Nothing
    _        -> undefined

step :: Int -> Int -> Int -> Int -> Grid -> IO (Maybe GuardState)
step gi gj h w grid = do
  r <- indexGrid h w (gi, gj) grid
  case r of
    Just '^' -> do stepNorthCase h w (gi, gj) grid
    Just '>' -> do stepEastCase h w (gi, gj) grid
    Just 'v' -> do stepSouthCase h w (gi, gj) grid
    Just '<' -> do stepWestCase h w (gi, gj) grid
    Nothing  -> return Nothing
    Just c   -> error [c]

resultsInLoop :: PathData -> Int -> Int -> Int -> Int -> Grid -> IO Bool
resultsInLoop p gi gj h w grid = do
  r <- step gi gj h w grid
  case r of
    Just gs@(o, (gi', gj')) -> if gs `elem` p then
                                 return $ True
                               else
                                 resultsInLoop (gs:p) gi' gj' h w grid
    Nothing                 -> return $ False


play :: PathData -> Int -> Int -> Int -> Int -> Grid -> IO PathData
play p gi gj h w grid = do
  r <- step gi gj h w grid
  case r of
    Just gs@(o, (gi', gj'))  -> play (gs:p) gi' gj' h w grid
    Nothing  -> return p

printRow :: Int -> Int -> Grid -> IO ()
printRow w i grid = do
  mapM_ (\j -> readArray grid (i, j) >>= putStr . return) [0..w-1]

printGrid :: Int -> Int -> Grid -> IO ()
printGrid h w grid = mapM_ (\i -> printRow w i grid >> putStrLn "") [0..h-1]

solution :: Array (Int, Int) Char
         -> [(Int, Int)] -> Orientation -> Int -> Int -> Int -> Int -> Grid
         -> IO Int
solution og op o gi gj h w grid = do
  let op' = init . init $ op
  k <- tryObstacles $ filter (\(i, j) -> (i, j) /= (gi, gj) && (i, j) /= northCell (gi, gj)) (nub op')
  return k
    where
      tryObstacles :: [(Int, Int)] -> IO Int
      tryObstacles []          = return 0
      tryObstacles ((i,j):bs)  = do
        grid <- thaw og
        Part1.update (i, j) '#' grid
        r <- resultsInLoop [(o, (gi, gj))] gi gj h w grid
        let k = if r then 1 else 0
        print r
        n <- tryObstacles bs
        return $ n + k

--}

isGuard :: Char -> Bool
isGuard c = isJust $ getGuard c

orientationOfGuard :: Char -> Orientation
orientationOfGuard '^' = North
orientationOfGuard '>' = East
orientationOfGuard 'v' = South
orientationOfGuard '<' = West

reset :: Array (Int, Int) Char -> IO Grid
reset og = thaw og

main :: IO ()
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let ls    = lines content
  let height = length ls
  let width  = length $ head ls
  let ix :: ((Int, Int), (Int, Int)) = ((0, 0), (height-1, width-1))

  let gi = fromJust $ findIndex Part1.containsGuard ls
  let gj = fromJust $ findIndex Part1.isGuard (ls !! gi)

  grid :: Grid <- newArray ix '0'
  grid0 :: Grid <- newArray ix '0'

  Part1.initArray height width ls grid
  Part1.initArray height width ls grid0

  grid1 <- freeze grid

  t <- Part1.indexGrid height width (gi, gj) grid

  case t of
    Just c  -> do let o = orientationOfGuard c
                  op <- play [(o, (gi, gj))] gi gj height width grid0
                  result <- solution grid1 (snd <$> op) o gi gj height width grid
                  print result
    Nothing -> undefined

  hClose handle
