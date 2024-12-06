module Part1 where

import Data.Char
import Data.List
import Data.Maybe
import Data.String
import System.IO   (hClose, hGetContents, openFile, IOMode(ReadMode))

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

index :: (Int, Int) -> [[Char]] -> Maybe Char
index (i, j) grid = if i >= 0 && i < height && j >= 0 && j <  width then
                      Just $ grid !! i !! j
                   else
                     Nothing
  where
    height = length grid
    width  = length $ head grid

placeAt :: a -> Int -> [a] -> [a]
placeAt x i xs = let (ys, zs) = splitAt i xs in ys ++ x:tail zs

update :: (Int, Int) -> Char -> [[Char]] -> [[Char]]
update (i, j) c grid = placeAt line' i grid
  where
    line :: String
    line = grid !! i

    line' :: String
    line' = placeAt c j line

stepNorthCase :: (Int, Int) -> [[Char]] -> Maybe [[Char]]
stepNorthCase (i, j) grid =
  case northCell (i, j) `index` grid of
    Just '.' -> Just $ update (northCell (i, j)) '^' $ update (i, j) 'X' grid
    Just 'X' -> Just $ update (northCell (i, j)) '^' $ update (i, j) 'X' grid
    Just '#' -> Just $ update (i, j) '>' grid
    Nothing  -> Nothing
    _        -> undefined

stepEastCase :: (Int, Int) -> [[Char]] -> Maybe [[Char]]
stepEastCase (i, j) grid =
  case eastCell (i, j) `index` grid of
    Just '.' -> Just $ update (eastCell (i, j)) '>' $ update (i, j) 'X' grid
    Just 'X' -> Just $ update (eastCell (i, j)) '>' $ update (i, j) 'X' grid
    Just '#' -> Just $ update (i, j) 'v' grid
    Nothing  -> Nothing
    _        -> undefined

stepSouthCase :: (Int, Int) -> [[Char]] -> Maybe [[Char]]
stepSouthCase (i, j) grid =
  case southCell (i, j) `index` grid of
    Just '.' -> Just $ update (southCell (i, j)) 'v' $ update (i, j) 'X' grid
    Just 'X' -> Just $ update (southCell (i, j)) 'v' $ update (i, j) 'X' grid
    Just '#' -> Just $ update (i, j) '<' grid
    Nothing  -> Nothing
    _        -> undefined

stepWestCase :: (Int, Int) -> [[Char]] -> Maybe [[Char]]
stepWestCase (i, j) grid =
  case westCell (i, j) `index` grid of
    Just '.' -> Just $ update (westCell (i, j)) '<' $ update (i, j) 'X' grid
    Just 'X' -> Just $ update (westCell (i, j)) '<' $ update (i, j) 'X' grid
    Just '#' -> Just $ update (i, j) '^' grid
    Nothing  -> Nothing
    _        -> undefined

step :: [[Char]] -> Maybe [[Char]]
step grid =
  case guard of
    '^' -> stepNorthCase (gi, gj) grid
    '>' -> stepEastCase  (gi, gj) grid
    'v' -> stepSouthCase (gi, gj) grid
    '<' -> stepWestCase  (gi, gj) grid
    _   -> undefined
  where
    gi :: Int
    gi = fromJust $ findIndex containsGuard grid

    gj :: Int
    gj = fromJust $ findIndex isGuard (grid !! gi)

    guard :: Char
    guard = grid !! gi !! gj

play :: [[Char]] -> [[Char]]
play grid = case step grid of
              Just grid' -> play grid'
              Nothing    -> grid

solution :: [[Char]] -> Int
solution grid = 1 + (sum $ [ length $ filter (== 'X') line | line <- grid' ])
  where
    grid' :: [[Char]]
    grid' = play grid

printGrid :: [[Char]] -> IO ()
printGrid grid = mapM_ putStrLn grid

main :: IO ()
main = do
  handle  <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let grid = lines content
  print $ solution grid
  hClose handle
