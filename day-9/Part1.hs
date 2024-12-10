module Part1 where

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

main :: IO ()
main = do
  handle  <- openFile "day-9/input.txt" ReadMode
  content <- hGetContents handle
  let es  = symbolic $ (head $ lines content)
  let es' = rearrange 0 (reverse es) es
  print $ checksum 0 $ defragment es'
  print $ checksum 0 es'
  -- print $ checksum 0 $ defragment $ rearrange 0 (reverse es) es
  hClose handle
