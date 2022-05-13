{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use putStrLn" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use mapM_" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use when" #-}

module Lib2 (
  _size, interpolation, put, puts,  newTable,
  priceTableAfterStep, selStepsOnTable, findSamplesS, findSamplesT,
  randomStep, nextSteps, isCellEmpty, whoWon, _DEEP, getNextTable,
  Table, Pos,  Matr, Sample, Price, Val, Level
 ) where

import Control.Monad (sequence_, mapM_)
import Data.Array.IO --( getElems, readArray, writeArray, MArray(newArray), IOArray )
import Data.Foldable (Foldable(foldl'))
import Data.Time.Clock ( getCurrentTime, UTCTime(utctDayTime) )
import Data.Maybe ( fromJust, isJust )
import System.Random ( initStdGen, uniformR, StdGen )
import Consul
import System.IO ( hFlush, stdout )
import Control.Concurrent ( threadDelay )
import Data.List (sort, nub, nubBy)


type Table = IOArray Int Val
type Pos = (Int, Int)
type Matr = [Val]
type Sample = [Val]
type Price = Int
type Val = Char    -- 'x', 'o', ' '
type Level = Int   -- 0, 1, 2, 3
_size = 10

_WIDTH = 5
_DEEP = 3 :: Level
__debug = False

get :: Table -> Pos -> IO Val
get table (r, c) = readArray table (r * _size + c)

put :: Table -> Pos -> Val -> IO ()
put table (r, c) = writeArray table (r * _size + c)

puts :: Table -> [Int] -> Val -> IO ()
puts table ns v = mapM_ (\n -> put table (divMod n 10) v) ns

newTable :: IO Table
newTable = newArray (0, _size^2 - 1) ' '

isCellEmpty :: Table -> Pos -> IO Bool
isCellEmpty table (r, c) = do
  if r >= 0 && r < _size && c >= 0 && c < _size
    then do
      v <- get table (r, c)
      return $ v == ' '
    else
      return False

whoWon :: Table -> IO (Val, (Pos, Pos))
whoWon t = do
   matr <- getElems t
   let xs = findSample matr "xxxxx"
   let os = findSample matr "ooooo"
   if xs /= []
    then return ('x', head xs)
   else if os /= []
    then return ('o', head os)
    else return (' ', ((0,0), (0,0)))


-- UTILS ------------------------------------------------

interpolation :: (Pos, Pos) -> [Pos]
interpolation ((r1, c1), (r2, c2))
  | r1 == r2  = [(r1, c) | c <- [c1..c2] ]
  | c1 == c2  = [(r, c1) | r <- [r1..r2] ]
  | otherwise = [(r, c)  | (r, c) <- zip [r1..r2] [c1, c1 + dCol ..c2]]
  where
    dCol = signum (c2 - c1)

insertSepByN n [] sep = []
insertSepByN n xs sep = let (zs, rs) = splitAt n xs
               in zs ++ [sep] ++ insertSepByN n rs sep

swapXO cs = map f cs where
  f 'x' = 'o'
  f 'o' = 'x'
  f c = c

------------ образцы -------------------------------------------

-- samplesOs -- для оценки перспективности хода 'o'
_samplesOs :: [([Sample], Price)]
_samplesOs = [
  ([" oooo", "o ooo", "oo oo", "ooo o", "oooo "], 10),
  ([" xxxx", "x xxx", "xx xx", "xxx x", "xxxx "], 9),

  ([" ooo", "o oo", "oo o", "ooo "], 8),
  ([" xxx", "x xx", "xx x", "xxx "], 7),
  ([" oo", "o o", "oo "], 6),
  ([" xx", "x x", "xx "], 5),
  ([" o", "o "], 4),
  ([" x", "x "], 3)
  ]

-- samplesOs -- для оценки таблицы после хода 'o'
_samplesOt :: [([Sample], Price)]
_samplesOt = [
  (["ooooo"], 10^6),  
  ([" xxxx "], -10^6),   
  
  ([" oooo "], 10^5),     
  ([" xxxx", "x xxx", "xx xx", "xxx x", "xxxx "], -10^5), -- 'x' win in 1 step 

  ([" oooo", "o ooo", "oo oo", "ooo o", "oooo "], 1000),   -- 'o' win in 1 step

  (["  xxx", " x xx", " xx x", " xxx ", "x  xx", "x x x", "x xx ", "xx x ", "xxx  " ], -100), -- 'x' win in 2 steps  
  (["  ooo", " o oo", " oo o", " ooo ", "o  oo", "o o o", "o oo ", "oo o ", "ooo  " ], 100),   -- 'o' win in 2 steps

  (["xx   ", "x x  ", "x  x ", "x   x", " xx  ", " x x ", " x  x", "  x x", "   xx" ], -10),  -- 'x' win in 3 steps  
  (["oo   ", "o o  ", "o  o ", "o   o", " oo  ", " o o ", " o  o", "  o o", "   oo" ], 10)     -- 'o' win in 3 steps  
  ]

_samplesS :: Val -> [(Sample, Price)]
_samplesS val = let
  f (ss, p) = [(s, p) | s <- ss] :: [(Sample, Price)]
  pairs = concatMap f _samplesOs
 in
  if val == 'o'
   then pairs
   else map (\(s, p) -> (swapXO s, p)) pairs

_samplesT :: Val -> [(Sample, Price)]
_samplesT val = let
  f (ss, p) = [(s, p) | s <- ss] :: [(Sample, Price)]
  pairs = concatMap f _samplesOt
 in
  if val == 'o'
   then pairs
   else map (\(s, p) -> (swapXO s, p)) pairs :: [([Char], Price)]

----------- поиск образцов -------------------------

findSamplesS v matr = findSamples v (_samplesS v) matr
findSamplesT v matr = findSamples v (_samplesT v) matr

findSamples :: Val -> [(Sample, Price)] -> Matr -> [((Pos, Pos), Sample, Price)]
findSamples v samples matr = concatMap f samples
 where
   f :: (Sample, Price) -> [((Pos, Pos), Sample, Price)]
   f (sample, price)  = fmap (\pp -> (pp, sample, price)) (findSample matr sample)

findSample :: Matr -> Sample -> [(Pos, Pos)]
findSample matr xs = let
  n = length xs
  val r c = matr !! (r * _size + c)

  hor = [horEq r c | r <- [0.._size-1], c <- [0.._size-n] ]
  ver = [verEq r c | r <- [0.._size-n], c <- [0.._size-1] ]
  dia = [diaEq r c | r <- [0.._size-n], c <- [0.._size-n] ]
  aid = [aidEq r c | r <- [0.._size-n], c <- [n-1.._size-1] ]

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

--------- цена таблицы после сделанного хода -------------------

priceTableAfterStep :: Val -> Table -> IO Price
priceTableAfterStep v t = do
  matr <- getElems t
  let ps = findSamples v (_samplesT v) matr
  let price = sum [p | (pp, s, p) <- ps]
  return price

--------- выбор подходящих ходов v из заданной таблицы ------------------

selStepsOnTable :: Val -> Table -> IO [(Price, Pos)]
selStepsOnTable v t = do
  matr <- getElems t
  let samples = findSamples v (_samplesS v) matr  :: [((Pos, Pos), Sample, Price)]
  if null samples
    then randomStep v t
    else return $ getPricedSteps samples

getPricedSteps :: [((Pos, Pos), Sample, Price)] -> [(Price, Pos)]
            -- ps =  [(10,(8,5)),(10,(3,5))]
getPricedSteps ps = (take _WIDTH . nubBy (\a b -> snd a == snd b).reverse . sort) ps'
  where
 -- ps' = [(10,(3,5)),(8,(8,5)),(10,(8,5)),(6,(8,5)),(8,(3,5))]"
    ps' = concatMap pricedStep ps :: [(Price, Pos)]
    pricedStep :: ((Pos, Pos), Sample, Price) -> [(Price, Pos)]
    pricedStep ((pos1, pos2), sample, price) =
      [(price, pos) | (pos, s) <- zip (interpolation (pos1, pos2)) sample, s == ' ']
__pwp prompt x = if __debug
   then putStrLn ("---" ++ prompt ++"  "++ show x ++ " >") >> getLine >> return ()
   else return ()
--------------------------------------------------------------

-- несколько варианов лучшего хода игрока
nextSteps :: Val -> Table -> Level -> IO [(Price, Pos)]
nextSteps v table 0 = do
  steps <- selStepsOnTable v table :: IO [(Price, Pos)]

  if fst (head steps) == 10
    then return steps            -- есть выигрышный ход
    else do
      -- tables - список таблиц со сделанными ходами 
      tables <- mapM (getNextTable v table) steps  :: IO [Table]
      prices <- mapM (priceTableAfterStep v) tables :: IO [Price]
      --__pwp "prices" prices
      -- соединяем цены таблиц с ходами игрока
      let pps =  [(price, pos) | (price, (pr, pos))<- zip prices steps] :: [(Price, Pos)]
      let sortedPps = (reverse . sort) pps :: [(Price, Pos)]
      __pwp "steps level 0"  steps
      return sortedPps

-- несколько вариантов лучшего хода игрока

nextSteps v table level = do
  steps <- selStepsOnTable v table :: IO [(Price, Pos)]
  __pwp ("steps level " ++ show level) steps
  if fst (head steps) == 10
   then return steps            -- есть выигрышный ход
   else do
    -- tables - список таблиц со сделанными ходами 
    tables <- mapM (getNextTable v table) steps  :: IO [Table]
    let v' = if v == 'o' then 'x' else 'o'
    let f t = nextSteps v' t (level-1) :: IO [(Price, Pos)]
    let contrStep t = head <$> (reverse . sort <$> f t) :: IO (Price, Pos)
    -- лучшие ходы противника из всех таблиц tables
    contrSteps <- mapM contrStep tables :: IO [(Price, Pos)]
    __pwp "contrSteps" contrSteps
    -- сопоставляем отриц. цены ходов противника ходам игрока
    let pps = zipWith (\(_, pos) (contrPr, _) -> (-contrPr, pos)) steps contrSteps :: [(Price, Pos)]
    -- располагаем в пор убывания цен
    let sortedPps = (reverse . sort) pps :: [(Price, Pos)]
    __pwp ("sortedPps " ++ show level) sortedPps
    return sortedPps

getNextTable :: Val -> Table -> (Price, Pos) -> IO Table
getNextTable v table (_, pos) = do
  t <- mapArray id table
  put t pos v;
  return t

randomStep :: Val -> Table -> IO [(Price, Pos)]
randomStep v t = do
   g0 <- initStdGen
   let (a, g1) = uniformR (2, 7) g0 :: (Int, StdGen)
   let (b, g) = uniformR (2, 7) g1 :: (Int, StdGen)
   empty <- isCellEmpty t (a, b)
   if empty
     then return [(0, (a, b))]
     else randomStep v t

