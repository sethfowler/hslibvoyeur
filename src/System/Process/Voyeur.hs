{-# LANGUAGE TypeSynonymInstances #-}

module System.Process.Voyeur
( FFI.VoyeurContext
, withVoyeur
, FFI.ObserveExecFlags(..)
, FFI.defaultObserveExecFlags
, FFI.ObserveExecHandler
, FFI.observeExec
, FFI.ObserveExitFlags(..)
, FFI.defaultObserveExitFlags
, FFI.ObserveExitHandler
, FFI.observeExit
, FFI.ObserveOpenFlags(..)
, FFI.defaultObserveOpenFlags
, FFI.ObserveOpenHandler
, FFI.observeOpen
, FFI.ObserveCloseFlags(..)
, FFI.defaultObserveCloseFlags
, FFI.ObserveCloseHandler
, FFI.observeClose
, FFI.prepareEnvironment
, HasPid
, startObserving
) where

import Control.Concurrent.MVar (readMVar)
import Control.Exception (bracket)
import System.Exit (ExitCode(..))
import System.Posix.Types (ProcessID)
import System.Process.Internals (ProcessHandle(..), ProcessHandle__(..))

import qualified System.Process.Voyeur.FFI as FFI

withVoyeur :: (FFI.VoyeurContext -> IO a) -> IO a
withVoyeur = bracket FFI.createContext FFI.destroyContext

class HasPid a where
  toPid :: a -> IO (Maybe ProcessID)

instance HasPid ProcessID where
  toPid = return . Just

-- Unfortunately, we have to reach into the internals of
-- System.Process for this one.

#if MIN_VERSION_process(1,2,0)

instance HasPid ProcessHandle where
  toPid (ProcessHandle m _) = do
    p <- readMVar m
    case p of
      (OpenHandle pid) -> return $ Just pid
      _                -> return Nothing
    
#else

instance HasPid ProcessHandle where
  toPid (ProcessHandle m) = do
    p <- readMVar m
    case p of
      (OpenHandle pid) -> return $ Just pid
      _                -> return Nothing

#endif

startObserving :: HasPid a => FFI.VoyeurContext -> a -> IO ExitCode
startObserving c p = do
  mayPid <- toPid p
  case mayPid of
    Just pid -> FFI.startObserving c pid
    Nothing  -> return $ ExitFailure (-1)
