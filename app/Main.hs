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
  let settings = if length args < 2 
                  then (1, 9) 
                  else (read (head args), read (args !! 1)  )
  logNew settings
  table <- newTable
  -----
  -- (settings, table) <- loadFromLog
  -----
  putStr clrscr
  drawTable table
  run table settings

-------------------------------------------------------
run :: Table -> Settings -> IO ()
run t d_w = do
  putStr' $ norm ++ rc 22 0 ++ showCur ++ "("++ show (fst d_w) ++" "++ show (snd d_w) ++") q-quit >"

  line <- getLine
  when ('q' `notElem` line)  (do
      case readMaybe line of
        Nothing -> run t d_w
        Just posXint -> twoMoves t d_w posXint
      )

twoMoves :: Table -> Settings -> Int -> IO ()
twoMoves t settings posXint = do
  let posX = divMod posXint 10
  isEmpty <- isCellEmpty t posX
  if not isEmpty
    then run t settings
    else do
      put t posX 'x'
      hilightPos t 'x' posX
      logStep 'x' posX
      winer <- whoWon t
      if fst winer == 'x'
        then epilog t winer
        else do
          positionsO <- nextSteps 'o' t settings
          let posO = snd $ head positionsO
          put t posO 'o'
          hilightPos t 'o' posO
          logStep 'o' posO
          winer <- whoWon t
          if fst winer == 'o'
            then epilog t winer
            else run t settings

epilog table winer = do
  hilightWin table winer
  putStr' $ norm ++ rc 22 0 ++ showCur ++  "Continue ? [y], n >"
  ans <- getLine
  when (ans /= "n") main



