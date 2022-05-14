{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Test where

import Data.Array.IO 
import Consul
import Draw
import Lib
-------------- Tables -----------------------------
t1 = do
  t <- newTable
  puts t [45,55,65,75] 'o'
  putStr clrscr
  drawTable t
  putStrLn $ norm ++ showCur
  return t

t2 = do
  t <- newTable
  puts t [01,03,05,07] 'x'
  putStr clrscr
  drawTable t
  putStrLn $ norm ++ showCur
  return t

t3 = do
  t <- newTable
  puts t [45,55,65,95] 'x'
  putStr clrscr
  drawTable t
  putStrLn $ norm ++ showCur
  return t

--------------- Tests ----------------------------------------
__pwp prompt x =  putStrLn ("---" ++ prompt ++"  "++ show x ++ " >") >> getLine >> return ()

-- findSamples --------- поиск образцов 

test0 :: IO Table -> IO ()
test0 table = do
  t <- table
  matr <- getElems t 
  let samples1 = findSamplesS 'o' matr
  __pwp "samples finded by o " samples1
  let samples2 = findSamplesS 'x' matr
  __pwp "samples finded by x " samples2
 
-- selStepsOnTable -- выбор подходящих ходов из заданной таблицы 

test1 table = do
  t <- table
  steps1 <- selStepsOnTable 'o' t 5
  __pwp "steps on table for O" steps1
  steps2 <- selStepsOnTable 'x' t 5
  __pwp "steps on table for X" steps2
  

-- priceTableAfterStep -- оценка таблицы после сделанного хода 

test2 table = do
  t <- table
  priceX <- priceTableAfterStep 'x' t
  __pwp "ater X step " priceX
  priceO <- priceTableAfterStep 'o' t
  __pwp "ater O step " priceO

test3 table = do
  t <- table
  tables <- mapM (getNextTable 'o' t) [(0, (0,0)), (0, (1,1)) ]
  drawTable (head tables)
  __pwp "getNextTable of O" ""



test4 lev table = do
  t <- table
  steps <- nextSteps 'o' t lev 
  __pwp "steps O on table " steps


