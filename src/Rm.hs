{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rm where

import System.Directory
import System.FilePath
import Data.Time.Clock

import Control.Applicative
import Control.Monad
--import Control.Monad.Trans.State
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Functor.Identity

newtype Gold = Gold Int
  deriving (Eq, Ord, Show, Num)

newtype RmMonad a = RMonad {
  runRmMonad :: StateT RmState IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState RmState)


data RmState = Rms {
  base      :: FilePath,
  tempdir   :: FilePath,
  rel_path  :: FilePath
}

runToIO :: RmMonad a -> RmState -> IO a
runToIO m s = liftM fst $ runStateT (runRmMonad m) s


-- TODO 29/8: Remove rel_path from RmState?

getFullPath :: String -> RmMonad FilePath
getFullPath name = do
  state <- get
  return $ joinPath [base state, rel_path state, name]

getDestPath :: String -> RmMonad FilePath
getDestPath name = do
  state <- get
  return $ joinPath [tempdir state, rel_path state, name]

handleObject :: String -> RmMonad Bool
handleObject "." = return True
handleObject ".." = return True
handleObject obj = do
  path <- getFullPath obj
  file <- liftIO $ doesFileExist path
  if file then
          handleFile obj
  else
    do
      dir <- liftIO $ doesDirectoryExist path
      if dir then
             handleDir obj
      else
        return False

handleFile :: String -> RmMonad Bool
handleFile filename = do
  path <- getFullPath filename
  newpath <- getDestPath filename
  liftIO $ putStrLn $ "To move: " ++ path ++ " to: " ++ newpath
  writeMeta path newpath
  --liftIO $ renameFile path newpath  --TODO Seb 24/8: Actually move the files
  return True -- TODO: Catch exceptions in the IO monad and return a value based on that instead


-- TODO: 29/8 Cleanup dead code

handleDir :: String -> RmMonad Bool
handleDir dirname = do
  path <- getFullPath dirname
  newpath <- getDestPath dirname
  liftIO $ putStrLn $ "To move dir: " ++ path ++ " to: " ++ newpath
  --liftIO $ renameDirectory path newpath
  writeMeta path newpath

  --contents <- liftIO $ getDirectoryContents path
  --modify (\s -> s {rel_path=joinPath [rel_path s, dirname]})  -- Add the dirname to the relative path
  --res <- mapM handleObject contents
  -- TODO Seb 24/8: Remove the directory too
  return True

writeMeta :: FilePath -> FilePath -> RmMonad ()
writeMeta old new = do
  time <- liftIO $ getCurrentTime
  let str = old ++ "\n" ++ new ++ "\n" ++ show time
  liftIO $ writeFile (new ++ ".meta") str
