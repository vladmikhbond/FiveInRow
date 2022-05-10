module Draw (drawTable, hilightWin, hilightPos) where
import Data.Array.IO ( getElems, readArray, writeArray, MArray(newArray), IOArray )
import Consul
import System.IO ( hFlush, stdout )
import Control.Concurrent ( threadDelay )
import Lib
_cross = "><"
_zero = "<>"

drawTable :: Table -> IO ()
drawTable table = do
  putStr $ hideCur ++ rc 0 0
  cs <-  getElems table
  sequence_ [drawCell cs r c | r <- [0.._size - 1], c <- [0.._size - 1]]
  putStrLn ""

drawCell :: [Char] -> Int -> Int -> IO ()
drawCell cs row col
  | c == '\n' = putStrLn (rc row' col')
  | c == 'x'  = putStr $ rc row' (col'+1) ++ green  ++ _cross
  | c == 'o'  = putStr $ rc row' (col'+1) ++ red ++ _zero
  | otherwise = putStr $ rc row' (col'+1) ++ gray ++ show row ++ show col
  where
    i = _size * row + col
    c = cs !! i
    col' = (col + 1) * 3
    row' = row + 2

hilightWin :: Table -> (Char, (Pos, Pos)) -> IO ()
hilightWin table (who, segment) = do
   mapM_ f (interpolation segment)
 where
   simbol = if who == 'x' then _cross else _zero
   f (r, c) = putStr $ rc (r + 2) (c*3 + 4) ++ yellow  ++ simbol

hilightPos :: Table -> Char -> Pos -> IO ()
hilightPos t who (r, c) = do
  putStr $ rc r' c' ++ hideCur ++ colorB ++ color ++ sym
  threadDelay 200000
  putStr $ rc r' c' ++ blackB ++ color ++ sym 
 where 
    r' = r + 2
    c' = c * 3 + 4
    (color, colorB, sym) = if who == 'x' 
       then (red, redB, _cross) 
       else (green, greenB, _zero)

     


