{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Test where

import Data.Array.IO 
import Consul
import Draw
import Lib
import Trace ( load)
-------------- Tables -----------------------------
t1 = do
  (s, t) <- load "test/t1.txt"
  putStr clrscr
  drawTable t
  putStrLn $ norm ++ showCur
  return (s, t)

t2 = do
  (s, t) <- load "test/t2.txt"
  putStr clrscr
  drawTable t
  putStrLn $ norm ++ showCur
  return (s, t)

ooo = do
  t <- newTable
  puts t [33,34,35] 'o'
  putStr clrscr
  drawTable t
  putStrLn $ norm ++ showCur
  return ((1, 9), t)

oooo = do
  t <- newTable
  puts t [33,34,35,36] 'o'
  putStr clrscr
  drawTable t
  putStrLn $ norm ++ showCur
  return ((1, 9), t)

ooooo = do
  t <- newTable
  puts t [33,34,35,36,37] 'o'
  putStr clrscr
  drawTable t
  putStrLn $ norm ++ showCur
  return ((1, 9), t)

xoooo = do
  t <- newTable
  puts t [33,34,35,36] 'o'
  puts t [32] 'x'
  putStr clrscr
  drawTable t
  putStrLn $ norm ++ showCur
  return ((1, 9), t)


--------------- Tests ----------------------------------------


-- findSamples --------- поиск образцов 

test0 :: IO Table -> IO ()
test0 table = do
  t <- table
  matr <- getElems t 
  let samples1 = findSamplesS 'o' matr
  __trace "samples finded by o " samples1
  let samples2 = findSamplesS 'x' matr
  __trace "samples finded by x " samples2
 
-- selStepsOnTable -- выбор подходящих ходов из заданной таблицы 

test1 table = do
  (s, t) <- table
  steps1 <- selStepsOnTable 'o' t 5
  __trace "selStepsOnTable 'o'" steps1
  steps2 <- selStepsOnTable 'x' t 5
  __trace "selStepsOnTable 'x'" steps2
  
test2 table level = do
  (s, t) <- table
  (price, pos, table) <- selectStep t 'o' (level,  5) 
  __trace "selectStep 'o' on table " (price, pos)
  (priceX, posX, tableX) <- selectStep t 'o' (level,  5) 
  __trace "selectStep 'x' on table " (priceX, posX)
 

test3 table level = do
  (s, t) <- table
  (price, table) <- estimateTable t 'o' (level,  5) 
  __trace "estimateTable 'o' on table " price
  (priceX, tableX) <- estimateTable t 'x' (level,  5) 
  __trace "estimateTable 'x' on table " priceX

