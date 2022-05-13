{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use putStrLn" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use mapM_" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use tuple-section" #-}

module Lib2 (
  _size, interpolation, put, puts,  newTable,
  priceTableAfterStep, selStepsOnTable, findSamplesS, findSamplesT, 
  randomStep, nextSteps, isCellEmpty, whoWon,
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
import Data.List (sort, nub)


type Table = IOArray Int Val
type Pos = (Int, Int)
type Matr = [Val]
type Sample = [Val]
type Price = Int
type Val = Char    -- 'x', 'o', ' '
type Level = Int   -- 0, 1, 2, 3
_size = 10

_WIDTH = 5

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
  (["ooooo"], 10^6),   -- 'o' win in 0 step
  ([" xxxx", "x xxx", "xx xx", "xxx x", "xxxx "], -10^5), -- 'x' win in 1 step 
  ([" oooo "], 10^4),   -- 'o' win in 0 step
  
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
   else map (\(s, p) -> (swapXO s, p)) pairs

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

--------- выбор походящих ходов из заданной таблицы ------------------

selStepsOnTable :: Val -> Table -> IO [(Price, Pos)]
selStepsOnTable v t = do
  matr <- getElems t
  let samples = findSamples v (_samplesS v) matr  :: [((Pos, Pos), Sample, Price)]
  if null samples
    then randomStep v t
    else return $ getPricedSteps samples

getPricedSteps :: [((Pos, Pos), Sample, Price)] -> [(Price, Pos)]
getPricedSteps ps = (take _WIDTH . reverse . sort . nub) (concatMap pricedStep ps)
  where
    pricedStep :: ((Pos, Pos), Sample, Price) -> [(Price, Pos)]
    pricedStep ((pos1, pos2), sample, price) =
      [(abs price, pos) | (pos, s) <- zip (interpolation (pos1, pos2)) sample, s == ' ']

--------------------------------------------------------------
randomStep :: Val -> Table -> IO [(Price, Pos)]
randomStep v t = do
   g0 <- initStdGen
   let (a, g1) = uniformR (2, 7) g0 :: (Int, StdGen)
   let (b, g) = uniformR (2, 7) g1 :: (Int, StdGen)
   empty <- isCellEmpty t (a, b)
   if empty
     then return [(0, (a, b))]
     else randomStep v t

-- несколько вариантов лучшего хода игрока
nextSteps :: Val -> Table -> Level -> IO [(Price, Pos)]
nextSteps v table 0 = selStepsOnTable v table


-- несколько вариантов лучшего хода игрока
nextSteps v table level = do
  steps <- selStepsOnTable v table
  if fst (head steps) == 10000 
   then return steps            -- есть выигрышный ход
   else do 
    let v' = if v == 'o' then 'x' else 'o'
    --__pwp ("the best "++ [v] ++" steps") steps 
    -- tables - список таблиц со сделанными ходами 
    tables <- mapM (getNextTables v table) steps  :: IO [Table]
    -- самый лучший ход противника из таблицы tbl
    let contrSteps tbl  = head <$> nextSteps v' tbl (level-1) ::  IO (Price, Pos)
    -- лучшие ходы противника из всех таблиц tables
    steps' <- mapM contrSteps tables :: IO [(Price, Pos)]
    --__pwp "the best contra steps" steps' 
    -- соединяем цены ходов противника с ходами игрока
    let steps'' = zipWith (\(p, pos) (p', pos') -> (p', pos)) steps steps' ::  [(Price, Pos)]
    --__pwp "new prices" steps'' 
    -- выбираем 5 худших  
    let steps''' = sort steps''
    return steps'''

__pwp prompt x =  print (prompt ++ "  " ++ show x ++ "    >") >> getLine

getNextTables :: Val -> Table -> (Price, Pos) -> IO Table
getNextTables v table (_, pos) = do 
  t <- mapArray id table 
  put t pos v; 
  return t