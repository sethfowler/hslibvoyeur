{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module System.Process.Voyeur.FFI
( VoyeurContext
, createContext
, destroyContext
, ObserveExecFlags(..)
, defaultExecFlags
, ObserveExecHandler
, observeExec
, ObserveExitHandler
, observeExit
, ObserveOpenFlags(..)
, defaultOpenFlags
, ObserveOpenHandler
, observeOpen
, ObserveCloseHandler
, observeClose
, setResourcePath
, prepareEnvironment
, startObserving
) where

import Control.Applicative
import Control.Exception
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Word
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.Exit
import System.Posix.Types

--------------------------------------------------
-- Creating and destroying libvoyeur contexts.
--------------------------------------------------

-- | The context libvoyeur uses to store its state.
newtype VoyeurContext = VoyeurContext { unVoyeurContext :: Ptr () }

foreign import ccall unsafe "voyeur.h voyeur_context_create"
  voyeur_context_create :: IO (Ptr ())

createContext :: IO VoyeurContext
createContext = VoyeurContext <$> voyeur_context_create

foreign import ccall unsafe "voyeur.h voyeur_context_destroy"
  voyeur_context_destroy :: Ptr () -> IO ()

destroyContext :: VoyeurContext -> IO ()
destroyContext = voyeur_context_destroy . unVoyeurContext


--------------------------------------------------
-- Registering callbacks for particular events.
--------------------------------------------------

-- | Flags for observing 'exec' calls.
data ObserveExecFlags = ObserveExecFlags
  { observeExecCWD      :: !Bool  -- ^ True if you want the current working directory.
  , observeExecEnv      :: !Bool  -- ^ True if you want the environment. (Potentially slow.)
  , observeExecPath     :: !Bool  -- ^ True if you want the value of 'PATH'.
  , observeExecNoAccess :: !Bool  -- ^ True if you want to see calls that
                                  --   fail due to access restrictions.
  } deriving (Eq, Show)

-- | Default flags which observe the minimum amount of information.
defaultExecFlags :: ObserveExecFlags
defaultExecFlags = ObserveExecFlags False False False False

type ExecCallback = CString -> Ptr CString -> Ptr CString -> CString
                 -> CString -> CPid -> CPid -> Ptr () -> IO ()

foreign import ccall "wrapper"
  wrapExecCallback :: ExecCallback -> IO (FunPtr ExecCallback)

foreign import ccall unsafe "voyeur.h voyeur_observe_exec"
  voyeur_observe_exec :: Ptr () -> Word8 -> FunPtr ExecCallback -> Ptr () -> IO ()
                         
-- | A handler for 'exec' calls.
type ObserveExecHandler = BS.ByteString                     -- ^ The file being executed.
                       -> [BS.ByteString]                   -- ^ The arguments.
                       -> [(BS.ByteString, BS.ByteString)]  -- ^ The environment (if requested).
                       -> BS.ByteString                     -- ^ The value of 'PATH' (if requested).
                       -> BS.ByteString                     -- ^ The working directory
                                                            --   (if requested).
                       -> ProcessID                         -- ^ The new process ID.
                       -> ProcessID                         -- ^ The parent process ID.
                       -> IO ()

-- | Observe calls to the 'exec' and 'posix_spawn' families of functions.
observeExec :: VoyeurContext        -- ^ The context.
            -> ObserveExecFlags     -- ^ Flags controlling what will be observed.
            -> ObserveExecHandler   -- ^ A handler for observed 'exec*' events.
            -> IO ()
observeExec c (ObserveExecFlags {..}) h = do
  let flags =  asBit 0 observeExecCWD
           .|. asBit 1 observeExecEnv
           .|. asBit 2 observeExecPath
           .|. asBit 3 observeExecNoAccess

  h' <- wrapExecCallback $ \file argv envp path cwd pid ppid _ -> do
    file' <- safePackCString file
    argv' <- packCStringArray argv
    envp' <- packCStringArray envp
    path' <- safePackCString path
    cwd'  <- safePackCString cwd
    h file' argv' (map bsEnvSplit envp') path' cwd' pid ppid

  voyeur_observe_exec (unVoyeurContext c) flags h' nullPtr
    
-- Observing exit() calls.
type ExitCallback = CInt -> CPid -> CPid -> Ptr () -> IO ()

foreign import ccall "wrapper"
  wrapExitCallback :: ExitCallback -> IO (FunPtr ExitCallback)

foreign import ccall unsafe "voyeur.h voyeur_observe_exit"
  voyeur_observe_exit :: Ptr () -> Word8 -> FunPtr ExitCallback -> Ptr () -> IO ()
                         
-- | A handler for \'exit\' calls.
type ObserveExitHandler = ExitCode   -- ^ The exit status.
                       -> ProcessID  -- ^ The process ID of the exiting process.
                       -> ProcessID  -- ^ The parent process ID of the exiting process.
                       -> IO ()

-- | Observe calls to the \'exit\' family of functions.
observeExit :: VoyeurContext       -- ^ The context.
            -> ObserveExitHandler  -- ^ A handler for observed \'exit\' events.
            -> IO ()
observeExit c h = do
  h' <- wrapExitCallback $ \status pid ppid _ ->
    h (asExitCode status) pid ppid

  voyeur_observe_exit (unVoyeurContext c) 0 h' nullPtr

-- | Flags for observing \'open\' calls.
data ObserveOpenFlags = ObserveOpenFlags
  { observeOpenCWD :: !Bool  -- ^ True if you want the current working directory.
  } deriving (Eq, Show)

-- | Default flags which observe the minimum amount of information.
defaultOpenFlags :: ObserveOpenFlags
defaultOpenFlags = ObserveOpenFlags False

type OpenCallback = CString -> CInt -> CMode -> CString -> CInt -> CPid -> Ptr () -> IO ()

foreign import ccall "wrapper"
  wrapOpenCallback :: OpenCallback -> IO (FunPtr OpenCallback)

foreign import ccall unsafe "voyeur.h voyeur_observe_open"
  voyeur_observe_open :: Ptr () -> Word8 -> FunPtr OpenCallback -> Ptr () -> IO ()
                         
-- | A handler for \'open\' calls.
type ObserveOpenHandler = BS.ByteString  -- ^ The file being opened.
                       -> Int            -- ^ The flags used to open the file.
                       -> FileMode       -- ^ The mode. Only meaningful if O_CREAT
                                         --   was specified.
                       -> BS.ByteString  -- ^ The working directory (if requested).
                       -> Int            -- ^ The return value of \'open\'. May be a
                                         --   file descriptor or an error value.
                       -> ProcessID      -- ^ The process ID of the observed process.
                       -> IO ()

-- | Observe calls to \'open\'.
observeOpen :: VoyeurContext       -- ^ The context.
            -> ObserveOpenFlags    -- ^ Flags controlling what will be observed.
            -> ObserveOpenHandler  -- ^ A handler for observed \'open\' events.
            -> IO ()
observeOpen c (ObserveOpenFlags {..}) h = do
  let flags = (asBit 0 observeOpenCWD)

  h' <- wrapOpenCallback $ \path oflag mode cwd retval pid _ -> do
    path' <- safePackCString path
    cwd' <- safePackCString cwd
    h path' (fromIntegral oflag) mode cwd' (fromIntegral retval) pid

  voyeur_observe_open (unVoyeurContext c) flags h' nullPtr

-- Observing close() calls.
type CloseCallback = CInt -> CInt -> CPid -> Ptr () -> IO ()

foreign import ccall "wrapper"
  wrapCloseCallback :: CloseCallback -> IO (FunPtr CloseCallback)

foreign import ccall unsafe "voyeur.h voyeur_observe_close"
  voyeur_observe_close :: Ptr () -> Word8 -> FunPtr CloseCallback -> Ptr () -> IO ()
                         
-- | A handler for \'close\' calls.
type ObserveCloseHandler = Int        -- ^ The file descriptor being closed.
                        -> Int        -- ^ The return value of \'close\'.
                        -> ProcessID  -- ^ The process ID of the observed process.
                        -> IO ()

-- | Observe calls to \'close\'.
observeClose :: VoyeurContext        -- ^ The context.
             -> ObserveCloseHandler  -- ^ A handler for observed \'close\' events.
             -> IO ()
observeClose c h = do
  h' <- wrapCloseCallback $ \fd retval pid _ ->
    h (fromIntegral fd) (fromIntegral retval) pid

  voyeur_observe_close (unVoyeurContext c) 0 h' nullPtr


--------------------------------------------------
-- Other context configuration options.
--------------------------------------------------

foreign import ccall unsafe "voyeur.h voyeur_set_resource_path"
  voyeur_set_resource_path :: Ptr () -> CString -> IO ()

setResourcePath :: VoyeurContext -> FilePath -> IO ()
setResourcePath c path = withCString path $ \cPath ->
                           voyeur_set_resource_path (unVoyeurContext c) cPath

--------------------------------------------------
-- Observing processes.
--------------------------------------------------

foreign import ccall unsafe "voyeur.h voyeur_prepare"
  voyeur_prepare :: Ptr () -> Ptr CString -> IO (Ptr CString)

-- | Prepares an environment for a child process you want to observe.
-- 'prepareEnvironment' starts the server component of libvoyeur and
-- adds or modifies environment variables as necessary to inject code
-- into the child process you're about to create and make sure it can
-- connect to the server.
--
-- Generally after calling 'prepareEnvironment', you'll want to start
-- the child process using the returned environment, and then call
-- 'System.Process.Voyeur.startObserving' to begin receiving events.
--
-- If something goes wrong, 'prepareEnvironment' will return 'Nothing'.
prepareEnvironment :: VoyeurContext                  -- ^ The context.
                   -> [(String, String)]             -- ^ The environment you want to use.
                   -> IO (Maybe [(String, String)])  -- ^ A modified version of that
                                                     --   environment, or 'Nothing' if something
                                                     --   went wrong.
prepareEnvironment c envp = bracket newCEnvp freeCEnvp $ \envp' -> do
    cEnvp <- withArray0 nullPtr envp' (voyeur_prepare (unVoyeurContext c))
    if cEnvp == nullPtr
       then return Nothing
       else do envp'' <- peekCStringArray cEnvp
               free cEnvp
               return . Just $ map envSplit envp''
  where
    newCEnvp = mapM (newCString . envJoin) envp
    freeCEnvp = mapM free

foreign import ccall safe "voyeur.h voyeur_start"
  voyeur_start :: Ptr () -> CPid -> IO CInt

startObserving :: VoyeurContext -> ProcessID -> IO ExitCode
startObserving c pid = asExitCode <$> voyeur_start (unVoyeurContext c) pid


--------------------------------------------------
-- Utility functions.
--------------------------------------------------

asBit :: Int -> Bool -> Word8
asBit n True  = shiftL 1 n
asBit _ False = 0

asExitCode :: CInt -> ExitCode
asExitCode 0 = ExitSuccess
asExitCode s = ExitFailure (fromIntegral s)

safePackCString :: CString -> IO BS.ByteString
safePackCString s | s == nullPtr = return BS.empty
                  | otherwise    = BS.packCString s

safePeekCString :: CString -> IO String
safePeekCString s | s == nullPtr = return ""
                  | otherwise    = peekCString s

packCStringArray :: Ptr CString -> IO [BS.ByteString]
packCStringArray array
  | array == nullPtr = return []
  | otherwise        = do array' <- peekArray0 nullPtr array
                          mapM safePackCString array'

peekCStringArray :: Ptr CString -> IO [String]
peekCStringArray array
  | array == nullPtr = return []
  | otherwise        = do array' <- peekArray0 nullPtr array
                          mapM safePeekCString array'

envJoin :: (String, String) -> String
envJoin (k, v) = concat [k, "=", v]

envSplit :: String -> (String, String)
envSplit = safeTailSnd . break (== '=')

bsEnvSplit :: BS.ByteString -> (BS.ByteString, BS.ByteString)
bsEnvSplit = bsSafeTailSnd . BC.break (== '=')
  
safeTailSnd :: (String, String) -> (String, String)
safeTailSnd (a, b) | null b    = (a, b)
                   | otherwise = (a, tail b)

bsSafeTailSnd :: (BS.ByteString, BS.ByteString) -> (BS.ByteString, BS.ByteString)
bsSafeTailSnd (a, b) | BS.null b = (a, b)
                     | otherwise = (a, BS.tail b)
