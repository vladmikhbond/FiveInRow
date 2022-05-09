{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use putStrLn" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use mapM_" #-}


module Lib (
  stepO, put, puts, isCellEmpty, drawTable, newTable, whoWon, hilightWin, hilightPosO, Table 
 ) where

import Control.Monad (sequence_, mapM_)
import Data.Array.IO ( getElems, readArray, writeArray, MArray(newArray), IOArray )
import Data.Foldable (Foldable(foldl'))
import Data.Time.Clock ( getCurrentTime, UTCTime(utctDayTime) )
import Data.Maybe ( fromJust, isJust )
import System.Random ( initStdGen, uniformR, StdGen )
import Consul 
import System.IO ( hFlush, stdout )


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
  putStr $ hideCur ++ rc 0 0 
  cs <-  getElems table
  sequence_ [drawCell cs r c | r <- [0..size - 1], c <- [0..size - 1]]
  putStrLn norm

drawCell :: [Char] -> Int -> Int -> IO ()
drawCell cs row col
  | c == '\n' = putStr $ rc row' col' ++ "\n"
  | c == 'x'  = putStr $ rc row' (col'+1) ++ green  ++ "><"
  | c == 'o'  = putStr $ rc row' (col'+1) ++ yellow ++ "<>"
  | otherwise = putStr $ rc row' (col'+1) ++ gray ++ show row ++ show col
  where
    i = size * row + col
    c = cs !! i
    col' = (col + 1) * 3
    row' = row + 2

hilightWin :: Table -> (Char, (Pos, Pos)) -> IO ()
hilightWin table (who, segment) = do
   mapM_ f (unpack segment)
   hFlush stdout
 where   
   simbol = if who == 'x' then "><" else "<>" 
   f (r, c) = putStr $ rc (r + 2) (c*3 + 4) ++ red  ++ simbol 

hilightPosO :: Table -> Pos -> IO ()
hilightPosO t (r, c) = putStr (rc (r + 2) (c*3 + 4) ++ cian  ++ "<>") >> hFlush stdout 


-- UTILS ------------------------------------------------

unpack ((r1, c1), (r2, c2)) 
  | r1 == r2  = [(r1, c) | c <- [c1..c2] ]
  | c1 == c2  = [(r, c1) | r <- [r1..r2] ]
  | otherwise = [(r, c)  | (r, c) <- zip [r1..r2] [c1, c1+dCol..c2]]
  where 
    dCol = signum (c2 - c1)

insertSepByN n [] sep = []
insertSepByN n xs sep = let (zs, rs) = splitAt n xs
               in zs ++ [sep] ++ insertSepByN n rs sep
---------------------------------------------------------

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


findSamples :: Matr -> [Sample] -> [(Pos, Pos, Sample)]
findSamples matr samples = concatMap f samples 
 where
   f sample  = (\(p1, p2) -> (p1, p2, sample)) <$> findSample matr sample

getPotentPos :: (Pos, Pos, Sample) -> [Pos]     -- getPotentPos ((1, 5), (4, 2), "x x ")  ->  [(2,4),(4,2)]
getPotentPos ((r1, c1), (r2, c2), sample) 
  | r1 == r2  = [(r1, c) | (c, s) <- zip [c1..c2] sample, s == ' ']
  | c1 == c2  = [(r, c1) | (r, s) <- zip [r1..r2] sample, s == ' ']
  | otherwise = [(r, c)  | (r, c, s) <- zip3 [r1..r2] [c1, c1+dCol..c2] sample, s == ' ']
 where 
  dCol = signum (c2 - c1)

getPotentPoses :: [(Pos, Pos, Sample)] -> [Pos]  -- getPotentPoses [((1, 5), (4, 2), "x x ")]  ->  [(2,4),(4,2)]
getPotentPoses trios = concatMap getPotentPos trios
    
isCellEmpty :: Table -> Pos -> IO Bool
isCellEmpty table (r, c) = do
  if r >= 0 && r < size && c >= 0 && c < size
    then do
      v <- get table (r, c)
      return $ v == ' '
    else
      return False

whoWon :: Table -> IO (Char, (Pos, Pos))  -- 'x', 'o', ' '
whoWon t = do
   matr <- getElems t
   let xs = findSample matr "xxxxx"
   let os = findSample matr "ooooo"
   if xs /= []
   then return ('x', head xs)
   else if os /= []
   then return ('o', head os)
   else return (' ', ((0,0), (0,0)))
-------------------------------------------------------------

samples_ = [
  " oooo", "o ooo", "oo oo", "ooo o", "oooo ", 
  " xxxx", "x xxx", "xx xx", "xxx x", "xxxx ",

  " ooo", "o oo", "oo o", "ooo ",  
  " xxx", "x xx"," xx x", "xxx ",
  
   " oo", "o o", "oo ",
   " o", "o " ,

   " xx", "x x", "xx "
  ]

stepO :: Table -> IO Pos
stepO t = do
  matr <- getElems t

  let trios = findSamples matr samples_
  let empties = getPotentPoses trios
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







{-- 
  знаходимо межі всіх зразків, впорядковані по зменшості ціни - findSamples
  серед знайдених меж обираємо першу, яка припадає на вільне поле
  ставимо в неї "о"                              
--}


