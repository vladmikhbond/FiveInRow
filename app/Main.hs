module Main where


import Data.Array.IO ()
import Control.Monad ( when )
import Text.Read ( readMaybe )
import Lib2
import Consul
import Draw
import System.IO
    ( hClose, hFlush, hPutStr, openFile, stdout, IOMode(AppendMode, WriteMode) )

_LOG_TXT = "LOG.txt"
logStep :: Val -> Pos -> IO ()
logStep v (r, c) = do
  h <- openFile _LOG_TXT AppendMode
  hPutStr h $ v : show (r * 10 + c)
  hClose h

logNew = do
  h <- openFile _LOG_TXT WriteMode
  hClose h

loadFromLog = do
  line <- readFile _LOG_TXT
  let vps = f line
  t <- newTable
  mapM_ (g t) vps
  return t
 where
  g t (v, (r, c)) = put t (r, c) v

  f :: [Char] -> [(Val,Pos)]
  f (v : r : c : rest) = (v, (read [r], read [c])) : f rest
  f _ = []
------------------------------------------------------
main :: IO ()
main = do
  -- t <- newTable
  -- logNew
  t <- loadFromLog
  putStr clrscr
  drawTable t
  run t (2, 5)


-------------------------------------------------------
run :: Table -> Settings -> IO ()
run t d_w = do
  putStr $ norm ++ rc 12 0 ++ showCur ++ "("++ show (fst d_w) ++" "++ show (snd d_w) ++") q-quit >"
  hFlush stdout

  s <- getLine
  when ('q' `notElem` s)  (do
      let nn = readMaybe s :: Maybe Int
      case nn of
        Nothing -> run t d_w
        Just posXint -> twoMoves t d_w posXint
       )

twoMoves :: Table -> Settings -> Int -> IO ()
twoMoves t d_w posXint = do
  let posX = divMod posXint 10
  isEmpty <- isCellEmpty t posX
  if not isEmpty
    then run t d_w
    else do
      put t posX 'x'
      hilightPos t 'x' posX
      logStep 'x' posX
      who <- whoWon t
      if fst who == 'x'
        then epilog t who
        else do
          poses <- nextSteps 'o' t d_w  -- <<<<<<<<<<<<<<<<<
          let posO = snd $ head poses
          put t posO 'o'
          hilightPos t 'o' posO
          logStep 'o' posO
          who <- whoWon t
          if fst who == 'o'
            then epilog t who
            else run t d_w

epilog t who = do
  hilightWin t who
  putStr $ norm ++ rc 12 0 ++ showCur ++  "Continue ? [y], n >"
  hFlush stdout
  ans <- getLine
  when (ans /= "n") main



