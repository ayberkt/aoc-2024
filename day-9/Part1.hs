module Part1 where

import Data.List
import Data.String
import System.IO
import Data.Char
import Data.Maybe

import Utils

filesWithIDs :: String -> [(Int, Char)]
filesWithIDs s = zip files ((head . show) <$> [0..length files-1])
  where
    files :: [Int]
    files = parseDigit <$> fst <$> filter (even . snd) (zip s [0..length s-1])


spaces :: String -> [Int]
spaces s = parseDigit <$> fst <$> filter (odd . snd) (zip s [0..length s-1])

data Expr = FreeSpace Int | File Char Int deriving (Eq, Show)

symbolic :: String -> [Expr]
symbolic s = interweave xs ys
  where
    fis :: [(Int, Char)]
    fis = filesWithIDs s

    ns :: [Int]
    ns = spaces s

    xs :: [Expr]
    xs = [ File i f | (f, i) <- fis ]

    ys :: [Expr]
    ys = FreeSpace <$> ns

blocks :: String -> [Maybe Char]
blocks s = concat $ interweave xs ys
    where
      fis :: [(Int, Char)]
      fis = filesWithIDs s

      ns :: [Int]
      ns = spaces s

      xs :: [[Maybe Char]]
      xs = [ replicate f (Just i) | (f, i) <- fis ]

      ys :: [[Maybe Char]]
      ys = (flip replicate Nothing) <$> ns

nextMove :: [Maybe Char] -> Maybe Char
nextMove bs = let
                bs' = dropWhile isNothing (reverse bs)
              in
                if Nothing `elem` bs' then
                  if isJust (head bs') then head bs' else undefined
                else
                  Nothing
deleteLast :: [Maybe Char] -> [Maybe Char]
deleteLast (p:bs) = if all isNothing bs then Nothing : bs else p : deleteLast bs

organize :: [Maybe Char] -> [Maybe Char]
organize bs =
  case nextMove bs of
    Nothing -> bs
    Just c  -> let
                 bs' = deleteLast bs
               in
                 organize $ insertAt (fromJust (elemIndex Nothing bs')) (Just c) bs'

render :: [Maybe Char] -> String
render = map (\x -> case x of { Nothing -> '.' ; Just c -> c })

rearrange :: String -> String
rearrange = render . organize . blocks

solution :: String -> Int
solution s = sum [ beta c * i | (c, i) <- zip s' [0..length s'-1] ]
  where
    beta :: Maybe Char -> Int
    beta (Just c) = parseDigit c
    beta Nothing  = 0

    s' :: [Maybe Char]
    s' = organize . blocks $ s

isZeroSpace :: Expr -> Bool
isZeroSpace (FreeSpace 0) = True
isZeroSpace e = False

main :: IO ()
main = do
  handle  <- openFile "day-9/input.txt" ReadMode
  content <- hGetContents handle
  print $ length $ filter (not . isZeroSpace) $ symbolic $ (head $ lines content)
  hClose handle
