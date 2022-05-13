module Test where

import Control.Monad (sequence_, mapM_)
import Data.Array.IO --( getElems, readArray, writeArray, MArray(newArray), IOArray )
import Data.Foldable (Foldable(foldl'))
import Data.Time.Clock ( getCurrentTime, UTCTime(utctDayTime) )
import Data.Maybe ( fromJust, isJust )
import System.Random ( initStdGen, uniformR, StdGen )
import System.IO ( hFlush, stdout )
import Control.Concurrent ( threadDelay )
import Data.List (sort)
import Consul
import Draw
import Lib2

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

__pwp prompt x =  print $ prompt ++ show x

-- findSamples --------- поиск образцов 

test0 :: IO Table -> IO ()
test0 table = do
  t <- table
  matr <- getElems t 
  let samplesO = findSamplesT 'o' matr
  __pwp "samples finded by o " samplesO
  let samplesX = findSamplesT 'x' matr
  __pwp "samples finded by x " samplesX
  

-- priceTableAfterStep -- оценка таблицы после сделанного хода 

test1 table = do
  t <- table
  priceX <- priceTableAfterStep 'x' t
  __pwp "ater X step " priceX
  priceO <- priceTableAfterStep 'o' t
  __pwp "ater O step " priceO

-- selStepsOnTable -- выбор походящих ходов из заданной таблицы 

test2 table = do
  t <- table
  steps <- selStepsOnTable 'o' t
  __pwp "steps on table " steps

---------------------------------------------

test3 table = do
  t <- table
  steps <- nextSteps 'o' t 1 
  __pwp "steps on table " steps

