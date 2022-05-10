{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use putStrLn" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use mapM_" #-}


module Lib (
  _size, interpol, stepO, put, puts, isCellEmpty, newTable, whoWon,  Table, Pos
 ) where

import Control.Monad (sequence_, mapM_)
import Data.Array.IO ( getElems, readArray, writeArray, MArray(newArray), IOArray )
import Data.Foldable (Foldable(foldl'))
import Data.Time.Clock ( getCurrentTime, UTCTime(utctDayTime) )
import Data.Maybe ( fromJust, isJust )
import System.Random ( initStdGen, uniformR, StdGen )
import Consul
import System.IO ( hFlush, stdout )
import Control.Concurrent ( threadDelay )
import Data.List (sort)


type Table = IOArray Int Char
type Pos = (Int, Int)
type Matr = [Char]
type Sample = String
type Price = Int
_size = 10

get :: Table -> Pos -> IO Char
get table (i, j) = readArray table (i * _size + j)

put :: Table -> Pos -> Char -> IO ()
put table (i, j) = writeArray table (i * _size + j)

puts :: Table -> [Int] -> Char -> IO ()
puts table ns v = mapM_ (\n -> put table (divMod n 10) v) ns

newTable :: IO Table
newTable = newArray (0, _size^2 - 1) ' '

-- UTILS ------------------------------------------------

interpol ((r1, c1), (r2, c2))
  | r1 == r2  = [(r1, c) | c <- [c1..c2] ]
  | c1 == c2  = [(r, c1) | r <- [r1..r2] ]
  | otherwise = [(r, c)  | (r, c) <- zip [r1..r2] [c1, c1+dCol..c2]]
  where
    dCol = signum (c2 - c1)

insertSepByN n [] sep = []
insertSepByN n xs sep = let (zs, rs) = splitAt n xs
               in zs ++ [sep] ++ insertSepByN n rs sep

---------------------------------------------------------
-- _samples = [
--   " oooo", "o ooo", "oo oo", "ooo o", "oooo ",   -- 1000
--   " xxxx", "x xxx", "xx xx", "xxx x", "xxxx ",   --  999

--   " ooo ",                                       -- 100 
--   " xxx ",                                       --  99 

--   " ooo", "o oo", "oo o", "ooo ",                -- 50   
--   " xxx", "x xx"," xx x", "xxx ",                -- 49

--    " oo", "o o", "oo ",                            
--    " xx", "x x", "xx ",

--    " o", "o ",
--    " x", "x " 

--   ]

_pricedSamples :: [([Sample], Price)]
_pricedSamples = [
  ([" oooo", "o ooo", "oo oo", "ooo o", "oooo "], 1000),
  ([" xxxx", "x xxx", "xx xx", "xxx x", "xxxx "], 999),

  ([" ooo "], 100),
  ([" xxx "], 99),

  ([" ooo", "o oo", "oo o", "ooo "], 50),
  ([" xxx", "x xx"," xx x", "xxx "], 49),

   ([" oo", "o o", "oo "], 2),
   ([" xx", "x x", "xx "], 2),

  ([ " o", "o ", " x", "x "], 0)
  ]

_samples :: [Sample]
_samples = concatMap fst _pricedSamples

getSamplePrice :: Sample -> Price
getSamplePrice s = head [p | (ss, p) <- _pricedSamples, s `elem` ss]
--------------------------------------

-- getPricedSteps [((1, 5), (4, 2), "x x ")]  ->  [((2,4), 100),((4,2), 100)]
getPricedSteps :: [(Pos, Pos, Sample)] -> [(Price, Pos)]
getPricedSteps trios = (reverse . sort) (concatMap getPricedStep trios)
 where
  getPricedStep :: (Pos, Pos, Sample) -> [(Price, Pos)]
  getPricedStep (p1, p2, sample) =
    [(getSamplePrice sample, p) | (p, s) <- zip (interpol (p1, p2)) sample, s == ' ']



---------------------------------------------

findSamples :: Matr -> [(Pos, Pos, Sample)]
findSamples matr = concatMap f _samples
 where
   f sample  = (\(p1, p2) -> (p1, p2, sample)) <$> findSample matr sample

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



isCellEmpty :: Table -> Pos -> IO Bool
isCellEmpty table (r, c) = do
  if r >= 0 && r < _size && c >= 0 && c < _size
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
{-- 
  знаходимо всі зразки, які є в даному стані гри  
  
  знаходимо всі доцільні ходи "о"
  для кожного ходу 
      знаходимо все доцільні ходи "х"
      оцінюємо стан після кожного ходу "х"
  
  

--}




stepO :: Table -> IO Pos
stepO t = do
  steps <- getBestSteps t
  if null steps
    then rndStepO t
    else return $ snd (head steps)

getBestSteps :: Table -> IO [(Price, Pos)]
getBestSteps t = do
  matr <- getElems t
  let steps = getPricedSteps (findSamples matr)
  return $ take 5 steps


rndStepO :: Table -> IO Pos
rndStepO t = do
   g0 <- initStdGen
   let (a, g1) = uniformR (2, 7) g0 :: (Int, StdGen)
   let (b, g) = uniformR (2, 7) g1 :: (Int, StdGen)
   empty <- isCellEmpty t (a, b)
   if empty
     then return (a, b)
     else rndStepO t





