{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use putStrLn" #-}
{-# HLINT ignore "Eta reduce" #-}


module Lib (
  size, get, put, puts, newTable, drawTable, findSample, isCellEmpty, stepO,rndStepO, 
  test, getPotentPoses,
  Table, Pos, Matr, Sample ) where

import Data.Array.IO
    ( getElems, readArray, writeArray, MArray(newArray), IOArray )
import Consul
import Control.Monad (sequence_, mapM_)
import Data.Foldable (Foldable(foldl'))
import Data.Time.Clock ( getCurrentTime, UTCTime(utctDayTime) )
import Data.Functor ((<$>))
import Data.Maybe ( fromJust, isJust )
import System.Random

type Table = IOArray Int Char
type Pos = (Int, Int)
type Matr = [Char]
type Sample = String
size = 10

get :: Table -> Pos -> IO Char
get table (i, j) = readArray table (i * size + j)

put :: Table -> Pos -> Char -> IO ()
put table (i, j) = writeArray table (i * size + j)

puts :: Table -> [Int] -> Char -> IO ()
puts table ns v = mapM_ (\n -> put table (divMod n 10) v) ns

newTable :: IO Table
newTable = newArray (0, size^2 - 1) ' '

-- DRAWING -----------------------------------------------------

drawTable :: Table -> IO ()
drawTable table = do
  putStr clrscr
  cs <-  getElems table
  sequence_ [drawCell cs r c | r <- [0..size - 1], c <- [0..size - 1]]
  putStrLn norm

drawCell :: [Char] -> Int -> Int -> IO ()
drawCell cs row col
  | c == '\n' = putStr $ rc row' col' ++ "\n"
  | c == 'x'  = putStr $ rc row' (col'+1) ++ green  ++ [c,c]
  | c == 'o'  = putStr $ rc row' (col'+1) ++ yellow ++ [c,c]
  | otherwise = putStr $ rc row' (col'+1) ++ gray ++ show row ++ show col
  where
    i = size * row + col
    c = cs !! i
    col' = (col + 1) * 3
    row' = row + 2


-- UTILS ------------------------------------------------

insertSepByN n [] sep = []
insertSepByN n xs sep = let (zs, rs) = splitAt n xs
               in zs ++ [sep] ++ insertSepByN n rs sep
---------------------------------------------------------

--
findSample :: Matr -> Sample -> [(Pos, Pos)]
findSample matr xs = let
  n = length xs
  val r c = matr !! (r * size + c)

  hor = [horEq r c | r <- [0..size-1], c <- [0..size-n] ]
  ver = [verEq r c | r <- [0..size-n], c <- [0..size-1] ]
  dia = [diaEq r c | r <- [0..size-n], c <- [0..size-n] ]
  aid = [aidEq r c | r <- [0..size-n], c <- [n-1..size-1] ]

  horEq r c = if [val r (c+i)     | i <- [0..n-1]] /= xs then Nothing
    else Just ((r, c), (r, c+n-1))
  verEq r c = if [val (r+i) c     | i <- [0..n-1]] /= xs then Nothing
    else Just ((r, c), (r+n-1, c))
  diaEq r c = if [val (r+i) (c+i) | i <- [0..n-1]] /= xs then Nothing
    else Just ((r, c), (r+n-1, c+n-1))
  aidEq r c = if [val (r+i) (c-i) | i <- [0..n-1]] /= xs then Nothing
    else Just ((r, c), (r+n-1, c-n+1))
 in
  [ fromJust x |  x <- hor ++ ver ++ dia ++ aid, isJust x]


findSamples :: Matr -> [Sample] -> [(Pos, Pos)]
findSamples matr = concatMap (findSample matr)

findSamples2 :: Matr -> [Sample] -> [(Pos, Pos, Sample)]
findSamples2 matr samples = concatMap f samples 
 where
   f sample  = (\(p1, p2) -> (p1, p2, sample)) <$> findSample matr sample

getPotentPos :: (Pos, Pos, Sample) -> [Pos]     -- getPotentPos ((1, 5), (4, 2), "x x ")  ->  [(2,4),(4,2)]
getPotentPos ((r1, c1), (r2, c2), sample) = 
  [(r, c) | (r, c, s) <- zip3 [r1..r2] [c1, c1+dCol..c2] sample, s == ' ']
 where 
  dCol = signum (c2 - c1)

getPotentPoses :: [(Pos, Pos, Sample)] -> [Pos]     -- getPotentPoses [((1, 5), (4, 2), "x x ")]  ->  [(2,4),(4,2)]
getPotentPoses ts = concatMap getPotentPos ts


isCellEmpty :: Table -> Pos -> IO Bool
isCellEmpty table (r, c) = do
  if r >= 0 && r < size && c >= 0 && c < size
    then do
      v <- get table (r, c)
      return $ v == ' '
    else
      return False
-------------------------------------------------------------

-- tbl = do
--   t <- newTable
--   puts t [(2,2), (2,3), (2,4)] 'x'
--   --puts t [(2,2), (3,2), (4,2)] 'x'
--   --puts t [(1,1), (2,2), (3,3)] 'x'
--   puts t [(5,1), (4,2), (3,3), (2,4), (1,5)] 'x'

--   pos <- stepO t
--   return ()
samples = [
  " oooo", "o ooo", "oo oo", "ooo o", "oooo ", 
  " xxxx", "x xxx", "xx xx", "xxx x", "xxxx ", 
  " ooo", "ooo ", " xxx", "xxx ", " oo", "oo " , " xx", "xx ", " o", "o " 
  ]

stepO :: Table -> IO Pos
stepO t = do
  drawTable t
  lst <- getElems t
  let bounds = findSamples lst samples
  --putStr "bounds: " >> print bounds -------------------
  let ps = concat [ [p1, p2] | (p1, p2) <- bounds]
  empties <- filterEmptyPlaces t ps
  --putStr "empties: " >> print empties -------------------
  if null empties
    then rndStepO t
    else (return . head) empties
  
rndStepO :: Table -> IO Pos 
rndStepO t = do
   g0 <- initStdGen
   let (a, g1) = uniformR (0, 9) g0 :: (Int, StdGen) 
   let (b, g) = uniformR (0, 9) g1 :: (Int, StdGen) 
   empty <- isCellEmpty t (a, b) 
   if empty 
     then return (a, b)  
     else rndStepO t

test = do
  t <- newTable
  puts t [0..90] 'x'
  drawTable t
  p <- rndStepO t
  print p




filterEmptyPlaces :: Table -> [Pos] -> IO [Pos]
filterEmptyPlaces t [] = return []
filterEmptyPlaces t (p: ps) = do
  b <- isCellEmpty t p
  ps' <- filterEmptyPlaces t ps
  if b 
    then return (p : ps')
    else return ps'



-- stepO :: Table -> IO Pos
-- stepO table = do
--   rnd <- rnd100
--   let pos = rnd `divMod` 10
--   isEmpty <- isCellEmpty table pos
--   if isEmpty
--     then return pos 
--     else stepO table
{-- 
  знаходимо межі всіх зразків, впорядковані по зменшості ціни - findSamples
  серед знайдених меж обираємо першу, яка припадає на вільне поле
  ставимо в неї "о"                              
--}

