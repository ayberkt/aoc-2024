module Part1 where

import System.IO (IOMode(ReadMode), openFile, hGetContents, hClose)

import Utils (parseDigit)

data Block = Space Int
           | File String Int deriving (Eq, Show)

type SpaceSize = Int
type FileSize  = Int

parseDiskMap :: String -> [(String, FileSize, SpaceSize)]
parseDiskMap = parseRec 0
  where
    parseRec :: Int -> String -> [(String, FileSize, SpaceSize)]
    parseRec k [c]      = [(show k, parseDigit c, 0)]
    parseRec k (c:d:cs) = (show k, parseDigit c, parseDigit d) : parseRec (k+1) cs

toBlocks :: [(String, FileSize, SpaceSize)] -> [Block]
toBlocks = foldr (\(s, i, k) ps' -> File s i : Space k : ps') []

diskMapToBlocks :: String -> [Block]
diskMapToBlocks = toBlocks . parseDiskMap

-- Subtraction with remainder.
-- Gives the monus as the first component and the remainder as the second
-- component.
diff :: Int -> Int -> (Int, Int)
diff m n = let k = m - n in (max 0 k, abs (min 0 k))

erase :: Int -> [Block] -> [Block]
erase _ []            = []
erase 0 es            = es
erase k (Space 0:es)  = erase k es
erase k (File c 0:es) = erase k es
erase k (Space n:es)  = Space n : erase k es
erase k (File c n:es) = let (d, r) = diff n k in erase r (File c d : es)

eraseAtTheEnd :: Int -> [Block] -> [Block]
eraseAtTheEnd n = reverse . erase n . reverse

isFile :: Block -> Bool
isFile (File _ _) = True
isFile (Space _)  = False

isSpace :: Block -> Bool
isSpace (File _ _) = True
isSpace (Space _)  = False

-- The memory is fragmented if a file block occurs after a space block.
isFragmented :: [Block] -> Bool
isFragmented []            = False
isFragmented (File _ _:bs) = isFragmented bs
isFragmented (Space 0:bs)  = isFragmented bs
isFragmented (Space _:bs)  = any isFile bs

defragAux :: [Block] -> [Block] -> [Block]
defragAux _             []           = []
defragAux (File c n:ts) (Space 0:bs) = defragAux (File c n:ts) bs
defragAux (File c 0:ts) bs           = defragAux ts bs
defragAux (File c n:ts) (Space k:bs) = let
                                         (d, r) = k `diff` n
                                         bs'    = eraseAtTheEnd (n-r) bs
                                         move   = File c (n-r) -- chunk to move
                                         rem    = File c r     -- chunk remaining
                                       in
                                         if isFragmented (Space d:bs') then
                                           move : defragAux (rem:ts) (Space d:bs')
                                         else
                                           File c (n-r) : Space d : bs'
defragAux ts            (File c n:bs) = File c n : defragAux ts bs

defrag :: [Block] -> [Block]
defrag bs = defragAux (reverse (filter isFile bs)) bs

checksum :: [Block] -> Int
checksum = checksumRec 0
  where
    checksumRec :: Int -> [Block] -> Int
    checksumRec k []            = 0
    checksumRec k (Space 0:es)  = checksumRec k es
    checksumRec k (Space n:es)  = checksumRec k es
    checksumRec k (File _ 0:es) = checksumRec k es
    checksumRec k (File c n:es) = read c * k + checksumRec (k+1) (File c (n-1):es)

solution :: [String] -> Int
solution = checksum . defrag . diskMapToBlocks . head

run :: IO ()
run = do
  handle  <- openFile "day-9/input.txt" ReadMode
  content <- hGetContents handle
  print . solution $ lines content
  hClose handle
