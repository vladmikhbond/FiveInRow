module Main where

import Data.Array.IO ()
import Control.Monad ( when )
import Text.Read ( readMaybe )
import Lib
import Consul
import Draw
import System.IO
    ( hClose, hPutStr, openFile, IOMode(AppendMode, WriteMode) )
import System.Environment (getArgs)
import Trace
------------------------------------------------------
main :: IO ()
main = do
  -----
  args <- getArgs
  let sets = if length args < 2 
                  then (0, 5) 
                  else (read (head args), read (args !! 1)  )
  logNew sets
  table <- newTable
  -----
  -- (sets, table) <- loadFromLog  -- 
  -----
  putStr clrscr
  drawTable table
  run table sets

-------------------------------------------------------
run :: Table -> Settings -> IO ()
run t sets = do
  putStr' $ norm ++ rc 22 0 ++ showCur ++ 
            "("++ show (fst sets) ++" "++ show (snd sets) ++") q-quit >"

  line <- getLine
  when ('q' `notElem` line)  (do
      case readMaybe line of
        Nothing -> run t sets
        Just posXint -> twoMoves t sets (divMod posXint 10)
      )

twoMoves :: Table -> Settings -> Pos -> IO ()
twoMoves t sets posX = do
  isEmpty <- isCellEmpty t posX
  if not isEmpty
    then run t sets
    else do
      put t posX 'x'
      hilightPos t 'x' posX
      logStep 'x' posX
      winer <- whoWon t
      if fst winer == 'x'
        then epilog t winer
        else do
          (_, posO, _) <- selectStep t 'o' sets
          put t posO 'o'
          hilightPos t 'o' posO
          logStep 'o' posO
          winer <- whoWon t
          if fst winer == 'o'
            then epilog t winer
            else run t sets

epilog table winer = do
  hilightWin table winer
  putStr' $ norm ++ rc 22 0 ++ showCur ++  "Continue ? [y], n >"
  ans <- getLine
  when (ans /= "n") main



