module Rm where

import System.Directory
import System.FilePath

-- TODO Seb 22/7: ADd a custom state monad on top of IO to encapsulate settings

handleObject :: String -> IO Bool
handleObject "." = return True
handleObject ".." = return True
handleObject obj = do
  file <- doesFileExist obj
  if file then
          handleFile obj
  else
    do
      dir <- doesDirectoryExist obj
      if dir then
             handleDir obj
      else
        return False

handleFile :: String -> IO Bool
handleFile file = do
  home <- getHomeDirectory
  let tempdir = joinPath [home, ".rm.hs-temp"]
      newpath = joinPath [tempdir, file]
  renameFile file newpath
  return True -- TODO: Catch exceptions in the IO monad and return a value based on that instead

handleDir :: String -> IO Bool
handleDir dir = do
  contents <- getDirectoryContents dir
  -- TODO Join paths with dir
  putStrLn $ show contents
  res <- mapM handleObject contents
  return (and res)

