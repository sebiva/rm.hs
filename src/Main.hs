module Main where

import System.FilePath
import System.Directory

import Rm
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  cur_path <- getCurrentDirectory

  home <- getHomeDirectory
  mapM_ (rmObject home cur_path) args



rmObject :: FilePath -> FilePath -> FilePath -> IO Bool
rmObject home cur_path obj = do
  let (path, name) = splitFileName obj
      (pth, nm) = if name == "" then
                    (joinPath $ init $ splitPath path, last (splitPath path))
                  else
                    (path, name)
  putStrLn $ "Path: " ++ pth ++ ", Name: " ++ nm
  runToIO (handleObject nm) (Rms {tempdir=joinPath [home, ".rm.hs-temp"], base=joinPath [cur_path, pth],
                                  rel_path=""})


