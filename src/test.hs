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
  

-- priceTableAfterStep -- оценка таблицы после сделанного хода 

test2 table = do
  t <- table
  priceX <- priceTableAfterStep 'x' t
  __trace "ater X step " priceX
  priceO <- priceTableAfterStep 'o' t
  __trace "ater O step " priceO



test3 table level = do
  (s, t) <- table
  stepsO <- nextSteps 'o' t (level,  5) 
  __trace "nextSteps O on table " stepsO
  stepsX <- nextSteps 'x' t (level,  5) 
  __trace "nextSteps X on table " stepsX


