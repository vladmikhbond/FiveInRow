module Main where

import Lib
import Data.Array.IO
import  Control.Monad (when)
import Text.Read

main :: IO ()
main = do
  t <- newTable
  run t

-------------------------------------------------------
run :: Table -> IO ()
run t = do
  drawTable t
  putStrLn "q-quit >>"
  s <- getLine
  if 'q' `elem` s
    then return ()
    else do
      let mbx = readMaybe s :: Maybe Int
      case mbx of
        Nothing -> run t
        Just x -> do
          let posX = divMod x 10
          empty <- isCellEmpty t posX
          when empty $ do
              put t posX 'x'
              posO <- stepO t
              put t posO 'o'
          run t


-- test_findSample = do
--   t <- newTable
--   puts t [22, 23, 24] 'x'
--   puts t [51, 42, 33, 24, 15] 'x'
--   drawTable t
--   lst <- getElems t
--   let bounds = findSample lst " xxxxx "
--   print bounds
