module Main where

import Rm
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  mapM_ handleObject args

