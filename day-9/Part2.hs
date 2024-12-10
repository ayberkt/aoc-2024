module Part2 where

import Data.List
import Data.String
import System.IO
import Data.Char
import Data.Maybe

import Utils
import Utils (parseDigit)

data Expr = FreeSpace Int | File String Int deriving (Eq, Show)

newParseAux :: Int -> String -> [(String, Int, Maybe Char)]
newParseAux k [c]        = [(show k, parseDigit c, Nothing)]
newParseAux k (c1:c2:cs) = (show k, parseDigit c1, Just c2) : newParseAux (k+1) cs

gamma :: [(String, Int, Maybe Char)] -> [Expr]
gamma []                  = []
gamma ((s, i, Nothing):ps) = [File s i]
gamma ((s, i, Just c):ps) = File s i : FreeSpace (parseDigit c) : gamma ps

symbolic :: String -> [Expr]
symbolic = gamma . newParseAux 0

ignore :: String -> Int -> [Expr] -> [Expr]
ignore s j []               = []
ignore s 0 es               = es
ignore s k (FreeSpace 0:es) = ignore s k es
ignore s k (File c 0:es)    = ignore s k es
ignore s k (FreeSpace n:es) = FreeSpace n : ignore s k es
ignore s k (File c n:es)    | c == s    = ignore s (k-1) (File c (n-1):es)
ignore s k (File c n:es)    | otherwise = File c n : ignore s k es

dropEnd :: String -> Int -> [Expr] -> [Expr]
dropEnd s j xs = reverse $ ignore s j $ reverse xs

isFile :: Expr -> Bool
isFile (File _ _) = True
isFile (FreeSpace _) = False

rearrange :: Int -> [Expr] -> [Expr] -> [Expr] -> [Expr]
rearrange j acc es               []                        = []
rearrange j acc []               os                        = reverse acc ++ os
rearrange j acc (File c n:es)    (FreeSpace k:os) | n <= k = if null os then reverse acc ++ os else rearrange (j+n) (File c n:acc) es (FreeSpace (k - n):(dropEnd c n os))
rearrange j acc (File c n:es)    (FreeSpace k:os) | n > k  = if null os then reverse acc ++ os else rearrange (j+k) [] es (reverse acc ++ FreeSpace k:os)
rearrange j acc es               (File c n:os)             = rearrange j (File c n:acc) es os
rearrange j acc (FreeSpace n:es) os                        = rearrange (j+n) acc es os

checksum :: Int -> [Expr] -> Int
checksum k [] = 0
checksum k (FreeSpace 0:es) = checksum k es
checksum k (FreeSpace n:es) = checksum (k+1) es
checksum k (File c 0:es) = checksum k es
checksum k (File c n:es) = (read c * k) + checksum (k+1) (File c (n-1):es)

isZeroSpace :: Expr -> Bool
isZeroSpace (FreeSpace 0) = True
isZeroSpace e = False

isEmptySpace :: Expr -> Bool
isEmptySpace (FreeSpace _) = True
isEmptySpace e             = False

render :: [Expr] -> String
render [] = []
render (FreeSpace n:es) = replicate n '.' ++ render es
render (File s n:es) = concat (replicate n s) ++ render es

defragment :: [Expr] -> [Expr]
defragment [] = []
defragment (File c m:File c' n:es) | c == c' = defragment $ File c (m * n) : es
defragment (e:es) = e :  defragment es

newCount :: Int -> String -> Int
newCount k []     = 0
newCount k (c:cs) = k * parseDigit c + newCount (k+1) cs

isFragmented :: [Expr] -> Int
isFragmented es = length (dropWhile isFile es)

fileId :: Expr -> Int
fileId (FreeSpace _) = 0
fileId (File s n)    = - parseInt s

main :: IO ()
main = do
  handle  <- openFile "day-9/test.txt" ReadMode
  content <- hGetContents handle
  let es  = symbolic $ (head $ lines content)
  let es' = rearrange 0 [] (sortOn fileId $ reverse es) es
  print $ checksum 0 $ es'
  -- print $ checksum 0 $ defragment $ rearrange 0 (reverse es) es
  hClose handle
