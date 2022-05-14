
module Trace  ( logStep, logNew, loadFromLog) where

import Data.Array.IO ()
import Control.Monad ( when )
import Text.Read ( readMaybe )
import Lib
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
