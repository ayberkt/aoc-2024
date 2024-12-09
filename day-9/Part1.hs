module Part1 where

import Data.List
import Data.String
import System.IO
import Data.Char
import Data.Maybe

import Utils
import Utils (parseDigit)

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

ignore :: Int -> [Expr] -> [Expr]
ignore j []               = []
ignore 0 es               = es
ignore k (FreeSpace 0:es) = ignore k es
ignore k (File c 0:es)    = ignore k es
ignore k (FreeSpace n:es) = ignore (k-1) (FreeSpace (n-1):es)
ignore k (File c n:es)    = ignore (k-1) (File c (n-1):es)

dropEnd :: Int -> [Expr] -> [Expr]
dropEnd j xs = reverse $ ignore j $ reverse xs

isFile :: Expr -> Bool
isFile (File _ _) = True
isFile (FreeSpace _) = False

rearrange :: Int -> [Expr] -> [Expr] -> [Expr]
rearrange j es               []                        = []
rearrange j (File c n:es)    (FreeSpace k:os) | n <= k = if all isFile os  then os else File c n : rearrange (j+n) es (FreeSpace (k - n):(dropEnd n os))
rearrange j (File c n:es)    (FreeSpace k:os) | n > k  = if all isFile os then os else File c k : rearrange  (j+k) (File c (n-k):es) (dropEnd k os)
rearrange j es               (File c n:os)             = File c n : rearrange j es os
rearrange j (FreeSpace n:es) os                        = rearrange (j+n) es (dropEnd n os)

checksum :: Int -> [Expr] -> Int
checksum k [] = 0
checksum k (FreeSpace 0:es) = checksum k es
checksum k (FreeSpace n:es) = checksum k es
checksum k (File c 0:es) = checksum k es
checksum k (File c n:es) = (parseDigit c * k) + checksum (k+1) (File c (n-1):es)

isZeroSpace :: Expr -> Bool
isZeroSpace (FreeSpace 0) = True
isZeroSpace e = False

isEmptySpace :: Expr -> Bool
isEmptySpace (FreeSpace _) = True
isEmptySpace e             = False

render :: [Expr] -> String
render [] = []
render (FreeSpace n:es) = replicate n '.' ++ render es
render (File c n:es) = replicate n c ++ render es

defragment :: [Expr] -> [Expr]
defragment [] = []
defragment (File c m:File c' n:es) | c == c' = defragment $ File c (m * n) : es
defragment (e:es) = e :  defragment es

newCount :: Int -> String -> Int
newCount k []     = 0
newCount k (c:cs) = k * parseDigit c + newCount (k+1) cs

isFragmented :: [Expr] -> Int
isFragmented es = length (dropWhile isFile es)

main :: IO ()
main = do
  handle  <- openFile "day-9/input.txt" ReadMode
  content <- hGetContents handle
  let es  = symbolic $ (head $ lines content)
  let es' = rearrange 0 (reverse es) es
  print $ length $ es'
  print $ length $ defragment es'
  print $ length $ filter isEmptySpace es'
  print $ checksum 0 $ defragment es'
  print $ newCount 0 $ render es'
  print $ checksum 0 $ es'
  print $ newCount 0 $ render $ defragment $ defragment es'
  -- print $ checksum 0 $ defragment $ rearrange 0 (reverse es) es
  hClose handle
