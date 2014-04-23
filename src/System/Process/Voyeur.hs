{-# LANGUAGE TypeSynonymInstances #-}

-- | This package provides bindings to libvoyeur, a library for
-- observing the private activity of processes. Libvoyeur observes
-- a child process and all of its descendants, so it works even
-- when the child process calls out to other processes to do the
-- actual work.
--
-- To observe a process, use 'withVoyeur' to create an
-- 'FFI.VoyeurContext', then register handlers for the events you want
-- to observe using functions like 'FFI.observeExec'. When you've
-- set up all your handlers, use 'FFI.prepareEnvironment' to create a
-- special environment that will inject libvoyeur code into the
-- child process, and pass that environment to a function like
-- 'System.Process.runProcess'. Finally, pass the resulting 'ProcessHandle'
-- or 'ProcessID' to 'startObserving', and your handlers will be
-- called as events happen.
--
-- A simple function that prints a message every time a child
-- process opened a file might look like this:
--
-- > withVoyeur $ \ctx -> do
-- >   -- Set up a handler.
-- >   observeOpen ctx defaultOpenFlags $
-- >     \path _ _ _ _ pid -> putStrLn $ show pid ++ " opened " ++ show path
-- >
-- >   -- Set up the environment.
-- >   curEnv <- getEnvironment
-- >   newEnv <- prepareEnvironment ctx
-- >   
-- >   when (isJust newEnv) $ do
-- >     -- Start the child process.
-- >     handle <- runProcess program args Nothing newEnv Nothing Nothing Nothing
-- >
-- >     -- Observe it! startObserving only returns when the child process
-- >     -- exits, so we don't need to wait.
-- >     void $ startObserving ctx handle
--
-- A larger example program is included with the source code to this package.
module System.Process.Voyeur
(
-- * Observing a process
  withVoyeur
, FFI.prepareEnvironment
, startObserving

-- * Observing 'exec*' calls
, FFI.ObserveExecFlags(..)
, FFI.defaultExecFlags
, FFI.ObserveExecHandler
, FFI.observeExec

-- * Observing \'exit\' calls
, FFI.ObserveExitHandler
, FFI.observeExit

-- * Observing \'open\' calls
, FFI.ObserveOpenFlags(..)
, FFI.defaultOpenFlags
, FFI.ObserveOpenHandler
, FFI.observeOpen

-- * Observing \'close\' calls
, FFI.ObserveCloseHandler
, FFI.observeClose

-- * Types
, FFI.VoyeurContext
, HasPid
) where

import Control.Concurrent.MVar (readMVar)
import Control.Exception (bracket)
import System.Exit (ExitCode(..))
import System.FilePath
import System.Posix.Types (ProcessID)
import System.Process.Internals (ProcessHandle(..), ProcessHandle__(..))

import Paths_voyeur (getDataFileName)
import qualified System.Process.Voyeur.FFI as FFI

-- | Creates a 'FFI.VoyeurContext' and runs an IO action that observes
-- a process using it.
withVoyeur :: (FFI.VoyeurContext -> IO a) -> IO a
withVoyeur = bracket initContext FFI.destroyContext
 where
   initContext = do
     c <- FFI.createContext
     rPath <- getDataFileName $ "libvoyeur" </> "build"
     FFI.setResourcePath c (addTrailingPathSeparator rPath)
     return c

-- | The class of values that contain a 'ProcessID'. This is used to
-- abstract over the different representations of a process used by
-- the various process libraries.
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

-- | Start observing a child process. Your handlers will be called
-- while the process runs. Note that no handlers will be called if
-- you didn't start the process with an environment produced by
-- 'FFI.prepareEnvironment'.
--
-- When the child process exits, 'startObserving' will terminate the
-- server component of libvoyeur and return. This means that
-- 'startObserving' implicitly waits for the child process, so you
-- don't need to do this on your own.
startObserving :: HasPid a
               => FFI.VoyeurContext  -- ^ The context.
               -> a                  -- ^ The child process to observe.
               -> IO ExitCode        -- ^ The exit status of the child process.
startObserving c p = do
  mayPid <- toPid p
  case mayPid of
    Just pid -> FFI.startObserving c pid
    Nothing  -> return $ ExitFailure (-1)
