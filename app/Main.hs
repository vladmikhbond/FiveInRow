module Main where


import Data.Array.IO ()
import Control.Monad ( when )
import Text.Read ( readMaybe )
import Lib
import Consul
import Draw
import System.IO ( hFlush, stdout )

main :: IO ()
main = do
  t <- newTable
  -- puts t [01,03,05,07] 'x'
  -- puts t [45,55, 65,75] 'o'
  putStr clrscr
  drawTable t
  run t

-------------------------------------------------------
run :: Table -> IO ()
run t = do
  putStr $ norm ++ rc 12 0 ++ showCur ++ "q-quit >"
  hFlush stdout

  s <- getLine
  when ('q' `notElem` s)  (do
      let nn = readMaybe s :: Maybe Int
      case nn of
        Nothing -> run t
        Just x -> twoMoves t x
       )

twoMoves t nn = do
  let posX = divMod nn 10
  isEmpty <- isCellEmpty t posX
  if not isEmpty
    then run t
    else do
      put t posX 'x'
      hilightPos t 'x' posX   
      who <- whoWon t
      if fst who == 'x'
        then epilog t who
        else do
          posO <- stepO t 0   --------------
          put t posO 'o'
          hilightPos t 'o' posO  
          who <- whoWon t
          if fst who == 'o'
            then epilog t who
            else run t

epilog t who = do
  hilightWin t who
  putStr $ norm ++ rc 12 0 ++ showCur ++  "Continue ? [y], n >"
  hFlush stdout
  ans <- getLine
  when (ans /= "n") main



