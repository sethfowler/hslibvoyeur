{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module System.Process.Voyeur.FFI
( VoyeurContext
, createContext
, destroyContext
, ObserveExecFlags(..)
, defaultObserveExecFlags
, ObserveExecHandler
, observeExec
, ObserveExitFlags(..)
, defaultObserveExitFlags
, ObserveExitHandler
, observeExit
, ObserveOpenFlags(..)
, defaultObserveOpenFlags
, ObserveOpenHandler
, observeOpen
, ObserveCloseFlags(..)
, defaultObserveCloseFlags
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

-- Observing exec*() calls.
data ObserveExecFlags = ObserveExecFlags
  { observeExecCWD      :: !Bool
  , observeExecEnv      :: !Bool
  , observeExecNoAccess :: !Bool
  } deriving (Show)

defaultObserveExecFlags :: ObserveExecFlags
defaultObserveExecFlags = ObserveExecFlags False False False

type ExecCallback = CString -> Ptr CString -> Ptr CString -> CString
                 -> CPid -> CPid -> Ptr () -> IO ()

foreign import ccall "wrapper"
  wrapExecCallback :: ExecCallback -> IO (FunPtr ExecCallback)

foreign import ccall unsafe "voyeur.h voyeur_observe_exec"
  voyeur_observe_exec :: Ptr () -> Word8 -> FunPtr ExecCallback -> Ptr () -> IO ()
                         
type ObserveExecHandler = BS.ByteString -> [BS.ByteString] -> [(BS.ByteString, BS.ByteString)]
                       -> BS.ByteString -> ProcessID -> ProcessID -> IO ()

observeExec :: VoyeurContext -> ObserveExecFlags -> ObserveExecHandler -> IO ()
observeExec c (ObserveExecFlags {..}) h = do
  let flags =  (asBit 0 observeExecCWD)
           .|. (asBit 1 observeExecEnv)
           .|. (asBit 2 observeExecNoAccess)

  h' <- wrapExecCallback $ \path argv envp cwd pid ppid _ -> do
    path' <- safePackCString path
    argv' <- packCStringArray argv
    envp' <- packCStringArray envp
    cwd'  <- safePackCString cwd
    h path' argv' (map bsEnvSplit envp') cwd' pid ppid

  voyeur_observe_exec (unVoyeurContext c) flags h' nullPtr
    
-- Observing exit() calls.
data ObserveExitFlags = ObserveExitFlags
                         deriving (Show)

defaultObserveExitFlags :: ObserveExitFlags
defaultObserveExitFlags = ObserveExitFlags

type ExitCallback = CInt -> CPid -> CPid -> Ptr () -> IO ()

foreign import ccall "wrapper"
  wrapExitCallback :: ExitCallback -> IO (FunPtr ExitCallback)

foreign import ccall unsafe "voyeur.h voyeur_observe_exit"
  voyeur_observe_exit :: Ptr () -> Word8 -> FunPtr ExitCallback -> Ptr () -> IO ()
                         
type ObserveExitHandler = ExitCode -> ProcessID -> ProcessID -> IO ()

observeExit :: VoyeurContext -> ObserveExitFlags -> ObserveExitHandler -> IO ()
observeExit c _ h = do
  h' <- wrapExitCallback $ \status pid ppid _ ->
    h (asExitCode status) pid ppid

  voyeur_observe_exit (unVoyeurContext c) 0 h' nullPtr

-- Observing open() calls.
data ObserveOpenFlags = ObserveOpenFlags
  { observeOpenCWD      :: !Bool
  } deriving (Show)

defaultObserveOpenFlags :: ObserveOpenFlags
defaultObserveOpenFlags = ObserveOpenFlags False

type OpenCallback = CString -> CInt -> CMode -> CString -> CInt -> CPid -> Ptr () -> IO ()

foreign import ccall "wrapper"
  wrapOpenCallback :: OpenCallback -> IO (FunPtr OpenCallback)

foreign import ccall unsafe "voyeur.h voyeur_observe_open"
  voyeur_observe_open :: Ptr () -> Word8 -> FunPtr OpenCallback -> Ptr () -> IO ()
                         
type ObserveOpenHandler = BS.ByteString -> Int -> CMode -> BS.ByteString -> Int
                       -> ProcessID -> IO ()

observeOpen :: VoyeurContext -> ObserveOpenFlags -> ObserveOpenHandler -> IO ()
observeOpen c (ObserveOpenFlags {..}) h = do
  let flags = (asBit 0 observeOpenCWD)

  h' <- wrapOpenCallback $ \path oflag mode cwd retval pid _ -> do
    path' <- safePackCString path
    cwd' <- safePackCString cwd
    h path' (fromIntegral oflag) mode cwd' (fromIntegral retval) pid

  voyeur_observe_open (unVoyeurContext c) flags h' nullPtr

-- Observing close() calls.
data ObserveCloseFlags = ObserveCloseFlags
                         deriving (Show)

defaultObserveCloseFlags :: ObserveCloseFlags
defaultObserveCloseFlags = ObserveCloseFlags

type CloseCallback = CInt -> CInt -> CPid -> Ptr () -> IO ()

foreign import ccall "wrapper"
  wrapCloseCallback :: CloseCallback -> IO (FunPtr CloseCallback)

foreign import ccall unsafe "voyeur.h voyeur_observe_close"
  voyeur_observe_close :: Ptr () -> Word8 -> FunPtr CloseCallback -> Ptr () -> IO ()
                         
type ObserveCloseHandler = Int -> Int -> ProcessID -> IO ()

observeClose :: VoyeurContext -> ObserveCloseFlags -> ObserveCloseHandler -> IO ()
observeClose c _ h = do
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

prepareEnvironment :: VoyeurContext -> [(String, String)] -> IO [(String, String)]
prepareEnvironment c envp = bracket newCEnvp freeCEnvp $ \envp' -> do
    cEnvp <- withArray0 nullPtr envp' (voyeur_prepare (unVoyeurContext c))
    envp'' <- peekCStringArray cEnvp
    free cEnvp
    return $ map envSplit envp''
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
