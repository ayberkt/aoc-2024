module Part1 where

import Data.Array.IO

import Data.Char
import Data.List
import Data.Maybe
import Data.String
import System.IO   (hClose, hGetContents, openFile, IOMode(ReadMode))
import Data.Array (Array)

type Grid = IOUArray (Int, Int) Char

northCell :: (Int, Int) -> (Int, Int)
eastCell  :: (Int, Int) -> (Int, Int)
southCell :: (Int, Int) -> (Int, Int)
westCell  :: (Int, Int) -> (Int, Int)

northCell (i, j) = (i-1, j)
eastCell  (i, j) = (i, j+1)
southCell (i, j) = (i+1, j)
westCell  (i, j) = (i, j-1)

containsGuard :: String -> Bool
containsGuard s = '^' `elem` s || '>' `elem` s || 'v' `elem` s|| '<' `elem` s

getGuard :: Char -> Maybe Char
getGuard c | '^' == c  = Just c
           | '>' == c  = Just c
           | 'v' == c  = Just c
           | '<' == c  = Just c
           | otherwise = Nothing

isGuard :: Char -> Bool
isGuard c = isJust $ getGuard c

indexGrid :: Int -> Int -> (Int, Int) -> Grid -> IO (Maybe Char)
indexGrid h w (i, j) grid = do
  if i >= 0 && i < h && j >= 0 && j < w then do
    c <- readArray grid (i, j)
    return $ Just c
  else
    return Nothing

update :: (Int, Int) -> Char -> Grid -> IO ()
update (i, j) c grid = writeArray grid (i, j) c

stepNorthCase :: Int -> Int -> (Int, Int) -> Grid -> IO (Maybe (Int, Int))
stepNorthCase h w (i, j) grid = do
  let nc = northCell (i, j)
  result <- indexGrid h w nc grid
  case result of
    Just '.' -> do { update nc '^' grid; update (i, j) 'X' grid; return $ Just nc }
    Just 'X' -> do { update nc '^' grid; update (i, j) 'X' grid; return $ Just nc }
    Just '#' -> do { update (i, j) '>' grid; return $ Just (i, j) }
    Nothing  -> return Nothing
    _        -> undefined

stepEastCase :: Int -> Int -> (Int, Int) -> Grid -> IO (Maybe (Int, Int))
stepEastCase h w (i, j) grid = do
  let nc = eastCell (i, j)
  result <- indexGrid h w nc grid
  case result of
    Just '.' -> do { update nc '>' grid; update (i, j) 'X' grid; return $ Just nc }
    Just 'X' -> do { update nc '>' grid; update (i, j) 'X' grid; return $ Just nc }
    Just '#' -> do { update (i, j) 'v' grid; return $ Just (i, j) }
    Nothing  -> return Nothing
    _        -> undefined

stepSouthCase :: Int -> Int -> (Int, Int) -> Grid -> IO (Maybe (Int, Int))
stepSouthCase h w (i, j) grid = do
  let nc = southCell (i, j)
  result <- indexGrid h w nc grid
  case result of
    Just '.' -> do { update nc 'v' grid; update (i, j) 'X' grid; return (Just nc) }
    Just 'X' -> do { update nc 'v' grid; update (i, j) 'X' grid; return (Just nc) }
    Just '#' -> do { update (i, j) '<' grid; return (Just (i, j)) }
    Nothing  -> return Nothing
    _        -> undefined

stepWestCase :: Int -> Int -> (Int, Int) -> Grid -> IO (Maybe (Int, Int))
stepWestCase h w (i, j) grid = do
  let nc = westCell (i, j)
  result <- indexGrid h w (westCell (i, j)) grid
  case result of
    Just '.' -> do { update nc '<' grid; update (i, j) 'X' grid; return (Just nc) }
    Just 'X' -> do { update nc '<' grid; update (i, j) 'X' grid; return (Just nc) }
    Just '#' -> do { update (i, j) '^' grid; return $ Just (i, j) }
    Nothing  -> return Nothing
    _        -> undefined

step :: Int -> Int -> Int -> Int -> Grid -> IO (Maybe (Int, Int))
step gi gj h w grid = do
  r <- indexGrid h w (gi, gj) grid
  case r of
    Just '^' -> stepNorthCase h w (gi, gj) grid
    Just '>' -> stepEastCase  h w (gi, gj) grid
    Just 'v' -> stepSouthCase h w (gi, gj) grid
    Just '<' -> stepWestCase  h w (gi, gj) grid
    Nothing  -> return Nothing
    _   -> undefined

play :: Int -> Int -> Int -> Int -> Grid -> IO (Maybe (Int, Int))
play gi gj h w grid = do
  r <- step gi gj h w grid
  case r of
    Just (gi', gj') -> play gi' gj' h w grid
    Nothing         -> return Nothing

printGrid :: [[Char]] -> IO ()
printGrid grid = mapM_ putStrLn grid

initArray :: Int -> Int -> [[Char]] -> Grid -> IO ()
initArray h w ls grid =
  mapM_ (\(i, j) -> update (i, j) (ls !! i !! j) grid) coords
    where
      coords :: [(Int, Int)]
      coords = (,) <$> [0..h-1] <*> [0..w-1]

solution :: Int -> Int -> Int -> Int -> Grid -> IO Int
solution gi gj h w grid = do
  play gi gj h w grid
  elems <- getElems grid :: IO [Char]
  return $ 1 + (length $ filter (== 'X') elems)

main :: IO ()
main = do
  handle  <- openFile "day-6/input.txt" ReadMode
  content <- hGetContents handle
  let ls    = lines content
  let height = length ls
  let width  = length $ head ls
  let ix = ((0, 0), (height-1, width-1)) :: ((Int, Int), (Int, Int))

  let gi = fromJust $ findIndex containsGuard ls
  let gj = fromJust $ findIndex isGuard (ls !! gi)

  grid <- newArray ix '0' :: IO Grid 
  initArray height width ls grid

  result <- solution gi gj height width grid

  print result

  hClose handle
