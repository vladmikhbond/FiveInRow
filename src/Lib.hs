{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use putStrLn" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use mapM_" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use when" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Lib (
  _size, interpolation, put, puts,  newTable,
  priceTableAfterStep, selStepsOnTable, findSamplesS, findSamplesT,
  randomStep, nextSteps, isCellEmpty, whoWon,
  Table, Pos,  Matr, Sample, Price, Val, Level, Width, Settings
 ) where

import Control.Monad (sequence_, mapM_)
import Data.Array.IO --( getElems, readArray, writeArray, MArray(newArray), IOArray )
import Data.Foldable (Foldable(foldl'))
import Data.Time.Clock ( getCurrentTime, UTCTime(utctDayTime) )
import Data.Maybe ( fromJust, isJust )
import System.Random ( initStdGen, uniformR, StdGen )
import Consul
import Control.Concurrent ( threadDelay )
import Data.List (sort, nub, nubBy)

type Table = IOArray Int Val
type Pos = (Int, Int)
type Matr = [Val]
type Sample = [Val]
type Price = Int
type Val = Char    -- 'x', 'o', ' '
type Level = Int   -- 0..4
type Width = Int
type Settings = (Level, Width)

_size = 10

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

swapXO cs = [case c of {'x' -> 'o'; 'o' -> 'x'; c -> c } | c <- cs] 
eraseDots cs = [case c of {'.' -> ' '; c -> c } | c <- cs] 
contra v = case v of {'x' -> 'o'; 'o' -> 'x'}


__pwp prompt x = if __debug
   then putStrLn ("---" ++ prompt ++"  "++ show x ++ " >") >> getLine >> return ()
   else return ()

------------ образцы -------------------------------------------

----------------- samplesOs -- для оценки перспективности хода 'o'
-- точка запрещает ход в данную позицию

_samplesOs :: [([Sample], Price)]
_samplesOs = [
  ([" oooo", "o ooo", "oo oo", "ooo o", "oooo "], 10), 
  ([" xxxx", "x xxx", "xx xx", "xxx x", "xxxx "], 9),

   ([" xxx", "x xx", "xx x", "xxx "], 8),
 
  ([". ooo", "ooo ."], 7),
  ([" ooo", "o oo", "oo o", "ooo "], 7),
  
  ([" oo", "o o", "oo "], 6),
  ([" xx", "x x", "xx "], 5),
  ([" o", "o "], 4),
  ([" x", "x "], 3)
  ]

_samplesS :: Val -> [(Sample, Price)]
_samplesS val = let
  f (ss, p) = [(s, p) | s <- ss] :: [(Sample, Price)]
  pairs = concatMap f _samplesOs
 in
  if val == 'o'
   then pairs
   else map (\(s, p) -> (swapXO s, p)) pairs

------------------ samplesOs -- для оценки таблицы после хода 'o'

_samplesOt :: [([Sample], Price)]
_samplesOt = [
  (["ooooo"], 10^8), 

  ([" xxxx "], -10^7),    
  ([" xxxx", "x xxx", "xx xx", "xxx x", "xxxx "], -10^5), -- 'x' win in 1 step 
  ([" oooo "], 10^4),
  ([" oooo", "o ooo", "oo oo", "ooo o", "oooo "], 10^3),   -- 'o' win in 1 step

  ([" x xx ", " xx x "], -10^3),                          -- 'x' fork in 1 step
  ([" o oo ", " oo o "], 10^2),                           -- 'o' fork in 1 step
  
  
  (["  xxx", " x xx", " xx x", " xxx ", "x  xx", "x x x", "x xx ", "xx x ", "xxx  " ], -10), -- 'x' win in 2 steps  
  (["  ooo", " o oo", " oo o", " ooo ", "o  oo", "o o o", "o oo ", "oo o ", "ooo  " ], 1)   -- 'o' win in 2 steps

  --(["xx   ", "x x  ", "x  x ", "x   x", " xx  ", " x x ", " x  x", "  x x", "   xx" ], -10),  -- 'x' win in 3 steps  
  --(["oo   ", "o o  ", "o  o ", "o   o", " oo  ", " o o ", " o  o", "  o o", "   oo" ], 10)     -- 'o' win in 3 steps  
  ]

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

  horEq r c = if [f $ val r (c+i)     | i <- [0..n-1]] /= xs then Nothing
    else Just ((r, c), (r, c+n-1))
  verEq r c = if [f $ val (r+i) c     | i <- [0..n-1]] /= xs then Nothing
    else Just ((r, c), (r+n-1, c))
  diaEq r c = if [f $ val (r+i) (c+i) | i <- [0..n-1]] /= xs then Nothing
    else Just ((r, c), (r+n-1, c+n-1))
  aidEq r c = if [f $ val (r+i) (c-i) | i <- [0..n-1]] /= xs then Nothing
    else Just ((r, c), (r+n-1, c-n+1))

  f x = case x of {'.' -> ' '; x -> x}  

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

selStepsOnTable :: Val -> Table -> Width -> IO [(Price, Pos)]
selStepsOnTable v t width = do
  matr <- getElems t
  let samples = findSamples v (_samplesS v) matr  :: [((Pos, Pos), Sample, Price)]
  if null samples
    then randomStep v t
    else return $ getPricedSteps samples
 where
    getPricedSteps :: [((Pos, Pos), Sample, Price)] -> [(Price, Pos)]
                -- ps =  [(10,(8,5)),(10,(3,5))]
    getPricedSteps ps = (take width . nubBy (\a b -> snd a == snd b).reverse . sort) ps'
      where
    -- ps' = [(10,(3,5)),(8,(8,5)),(10,(8,5)),(6,(8,5)),(8,(3,5))]"
        ps' = concatMap pricedStep ps :: [(Price, Pos)]
        pricedStep :: ((Pos, Pos), Sample, Price) -> [(Price, Pos)]
        pricedStep ((pos1, pos2), sample, price) =
          [(price, pos) | (pos, s) <- zip (interpolation (pos1, pos2)) sample, s == ' ']

--------------------------------------------------------------

-- несколько варианов лучшего хода игрока (в порядке убыв. качества)

nextSteps :: Val -> Table -> Settings -> IO [(Price, Pos)]
nextSteps v t (level, width) = do
  steps <- selStepsOnTable v t width :: IO [(Price, Pos)]

  if fst (head steps) == 10
    then return steps            -- есть выигрышный ход
    else do
      -- цены новых таблиц, соотв. возможным ходам
      pricedTables <- mapM (\step -> estimateStepOnTable v (snd step) t) steps  :: IO [(Price, Table)]
      let tPrices = map fst pricedTables 
      -- соединяем цены таблиц с ходами игрока
      let pricedSteps = zipWith (\(_, pos) tPrice -> (tPrice, pos) ) steps tPrices
      -- 
      let sortPricedSteps = (reverse . sort) pricedSteps :: [(Price, Pos)]
      -- 
      if level == 0 
        then 
          return sortPricedSteps 
        else do -- каждую доч. таблицу оцениваем с т. з. противника 
          -- дочерние таблицы
          let tables = snd <$> pricedTables
          -- цены новых таблиц, соотв. возможным ходам
          pricedSteps2  <- mapM (\tbl -> nextSteps (contra v) tbl (level-1, width)) tables
          let tPrices2  = head <$> pricedSteps2
          -- соединяем цены таблиц с ходами игрока
          let pricedSteps2 = zipWith (\(_, pos) (tPrice, _) -> (-tPrice, pos) ) steps  tPrices2

          let sortPricedSteps2 = (reverse . sort) pricedSteps2 :: [(Price, Pos)] 
          return sortPricedSteps2


-- оценка состояния, создавшегося после хода pos игрока v в состоянии table 
estimateStepOnTable :: Val -> Pos -> Table -> IO (Price, Table)
estimateStepOnTable v pos table = do
  t <- mapArray id table
  put t pos v
  matr <- getElems t
  let ps = findSamples (contra v) (_samplesT v) matr
  let price = sum [p | (pp, s, p) <- ps]
  return (price, t)

-- случайный ход
randomStep :: Val -> Table -> IO [(Price, Pos)]
randomStep v t = do
   g0 <- initStdGen
   let (a, g1) = uniformR (2, 7) g0 :: (Int, StdGen)
   let (b, g) = uniformR (2, 7) g1 :: (Int, StdGen)
   empty <- isCellEmpty t (a, b)
   if empty
     then return [(0, (a, b))]
     else randomStep v t

