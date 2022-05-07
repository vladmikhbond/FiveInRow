module Main where

import Lib
import Data.Array.IO

main :: IO ()
main = do
  t <- newTable  
  run t
  

run :: IOArray Int Char -> IO ()
run t = do
  drawTable t
  putStrLn "q-quit >>" 
  s <- getLine
  if 'q' `elem` s 
    then return ()
    else do
      let (r, c) = divMod (read s) 10
      put t r c 'x'
      run t
