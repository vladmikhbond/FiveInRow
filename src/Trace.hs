
module Trace  ( logStep, logNew, load, loadFromLog, __trace) where

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

logNew (level, width) = do
  h <- openFile _LOG_TXT WriteMode
  hPutStr h $ show level ++ show width
  hClose h

loadFromLog :: IO (Settings, Table)
loadFromLog = load _LOG_TXT
  

load :: String -> IO (Settings, Table)
load path = do
  line <- readFile path
  let (d : w : rest) = line
  let valPoses = f rest
  t <- newTable
  mapM_ (g t) valPoses
  return ((read [d], read [w]), t)
 where
  f :: [Char] -> [(Val,Pos)]
  f (v : r : c : rest) = (v, (read [r], read [c])) : f rest
  f _ = []

  g t (v, (r, c)) = put t (r, c) v

