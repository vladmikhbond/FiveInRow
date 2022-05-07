{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use putStrLn" #-}
{-# LANGUAGE TupleSections #-}



module Lib ( size, get, put, puts, newTable, drawTable ) where

import Data.Array.IO
import Consul

import Control.Monad (sequence_, mapM_)

type Table = IOArray Int Char
type Pos = (Int, Int)

size = 10


get :: Table -> Int -> Int -> IO Char
get arr i j = readArray arr (i * size + j)

put :: Table -> Int -> Int -> Char -> IO ()
put arr i j = writeArray arr (i * size + j)

puts arr ps v = mapM_ (\(r, c) -> put arr r c v) ps

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
test = do
  t <- newTable
  
  --puts t [(2,2), (2,3), (2,4)] 'x'
  --puts t [(2,2), (3,2), (4,2)] 'x'
  --puts t [(1,1), (2,2), (3,3)] 'x'
  puts t [(5,1), (4,2), (3,3), (2,4), (1,5)] 'x'
  drawTable t
  lst <- getElems t 
  let bounds = findNx 5 'x' lst
  print bounds
 

--findNx :: Int -> Char -> [Char] -> Maybe (Pos, Pos) 
findNx :: Eq p => Int -> p -> [p] -> Maybe ((Int, Int), (Int, Int))
findNx n char tab = let 
   hor = [(r, c) | r <- [0..size-1], c <- [0..size-n] ]
   ver = [(r, c) | r <- [0..size-n], c <- [0..size-1] ]
   dia = [(r, c) | r <- [0..size-n], c <- [0..size-n] ]
   aid = [(r, c) | r <- [0..size-n], c <- [n-1..size-1] ]
   
   tst (r, c) = tab !! (r * size + c) == char

   tstH (r, c) = all (\x -> tst (r, c+x))   [0..n-1]
   tstV (r, c) = all (\x -> tst (r+x, c))   [0..n-1]
   tstD (r, c) = all (\x -> tst (r+x, c+x)) [0..n-1]
   tstA (r, c) = all (\x -> tst (r+x, c-x)) [0..n-1]
 
   hor' = dropWhile (not . tstH) hor
   ver' = dropWhile (not . tstV) ver
   dia' = dropWhile (not . tstD) dia
   aid' = dropWhile (not . tstA) aid

 in 
  if not (null hor')
  then let (a, b) = head hor' in Just ((a, b), (a, b + n - 1))
  else if not (null ver')
  then let (a, b) = head ver' in Just ((a, b), (a + n - 1, b))
  else if not (null dia')
  then let (a, b) = head dia' in Just ((a, b), (a + n - 1, b + n - 1))
  else if not (null aid')
  then let (a, b) = head aid' in Just ((a, b), (a + n - 1, b - n + 1))
  else Nothing

