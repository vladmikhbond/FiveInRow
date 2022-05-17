module Draw (drawTable, hilightWin, hilightPos) where
import Data.Array.IO ( getElems, readArray, writeArray, MArray(newArray), IOArray )
import Consul
    ( blackB,
      gray,
      green,
      greenB,
      hideCur,
      putStr',
      rc,
      red,
      redB,
      yellowB,
      white, black )

import Control.Concurrent ( threadDelay )
import Lib ( Pos, Table, _size, interpolation )
import Control.Monad (replicateM_)
_cross = "><"
_zero = "<>"

drawTable :: Table -> IO ()
drawTable table = do
  putStr $ hideCur ++ rc 0 0

  putStrLn " ------------------------------------------------- "
  replicateM_ n raw
  putStrLn "|    |    |    |    |    |    |    |    |    |    |"
  putStrLn " ------------------------------------------------- "

  cs <-  getElems table
  sequence_ [drawCell cs r c  | r <- [0..n], c <- [0..n]]
  putStrLn ""
 where
   n = _size - 1
   raw = putStrLn "|    |    |    |    |    |    |    |    |    |    |" >>
         putStrLn "|----|----|----|----|----|----|----|----|----|----|"



drawCell :: [Char] -> Int -> Int -> IO ()
drawCell matr row col
  | v == '\n' = putStrLn (rc row' col')
  | v == 'x'  = putStr $ rc row' (col'+1) ++ red ++ _cross
  | v == 'o'  = putStr $ rc row' (col'+1) ++ green ++ _zero
  | otherwise = putStr $ rc row' (col'+1) ++ gray ++ show row ++ show col
  where
    i = _size * row + col
    v = matr !! i
    row' = 2 * row + 2
    col' = 5 * col + 2


hilightPos :: Table -> Char -> Pos -> IO ()
hilightPos t who (r, c) = do
  putStr' $ rc r' c' ++ hideCur ++ yellowB ++ color ++ sym
  threadDelay 200000
  putStr' $ rc r' c' ++ colorB ++ color ++ sym
 where
    r' = 2 * r + 2
    c' = 5 * c + 3
    (color, colorB, sym) = if who == 'x'
       then (white, redB, _cross)
       else (white, greenB, _zero)


hilightWin :: Table -> (Char, (Pos, Pos)) -> IO ()
hilightWin table (who, segment) = do
   mapM_ f (interpolation segment)
 where
   simbol = if who == 'x' then _cross else _zero
   f (r, c) = putStr $ rc (2 * r + 2) (5 * c + 3) ++ yellowB ++ black ++ simbol



