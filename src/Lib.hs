{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use putStrLn" #-}

module Lib ( size, get, put, puts, newTable, drawTable ) where

import Data.Array.IO
import Consul

import Control.Monad (sequence_, mapM_)

size = 10

get :: IOArray Int Char -> Int -> Int -> IO Char
get arr i j = readArray arr (i * size + j)

put :: IOArray Int Char -> Int -> Int -> Char -> IO ()
put arr i j = writeArray arr (i * size + j)

puts arr ps v = mapM_ (\(r, c) -> put arr r c v) ps

newTable :: IO (IOArray Int Char)
newTable = newArray (0, size^2 - 1) ' '

-- DRAWING -----------------------------------------------------

drawTable :: IOArray Int Char -> IO ()
drawTable table = do
  putStr clrscr  
  cs <-  getElems table
  sequence_ [drawCell cs r c | r <- [0..size - 1], c <- [0..size - 1]]
  putStrLn norm

drawCell :: [Char] -> Int -> Int -> IO ()
drawCell cs row col 
  | c == '\n' = putStr $ rc row' col' ++ "\n"
  | c == 'x'  = putStr $ rc row' (col'+1) ++ green  ++ [c,c] 
  | c == 'o'  = putStr $ rc row' (col'+1) ++ yellow ++ [c,c] 
  | otherwise = putStr $ rc row' (col'+1) ++ gray ++ show row ++ show col 
  where 
    i = size * row + col
    c = cs !! i
    col' = (col + 1) * 3
    row' = row + 2


-- UTILS ------------------------------------------------

insertSepByN n [] sep = []
insertSepByN n xs sep = let (zs, rs) = splitAt n xs
               in zs ++ [sep] ++ insertSepByN n rs sep
