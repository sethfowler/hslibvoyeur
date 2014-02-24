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
, ObserveOpenFlags(..)
, defaultObserveOpenFlags
, ObserveOpenHandler
, observeOpen
, ObserveCloseFlags(..)
, defaultObserveCloseFlags
, ObserveCloseHandler
, observeClose
, prepareEnvironment
, startObserving
) where

import Control.Applicative
import Control.Exception
import Data.Bits
import qualified Data.ByteString as BS
import Data.Word
import Foreign
import Foreign.C.String
import Foreign.C.Types
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
                         
type ObserveExecHandler = BS.ByteString -> [BS.ByteString] -> [BS.ByteString] -> BS.ByteString
                       -> ProcessID -> ProcessID -> IO ()

observeExec :: VoyeurContext -> ObserveExecFlags -> ObserveExecHandler -> IO ()
observeExec c (ObserveExecFlags {..}) h = do
  let flags =  (asBit 0 observeExecCWD)
           .|. (asBit 1 observeExecEnv)
           .|. (asBit 2 observeExecNoAccess)

  h' <- wrapExecCallback $ \path argv envp cwd pid ppid _ -> do
    path' <- BS.packCString path
    argv' <- peekCStringArray argv
    envp' <- peekCStringArray envp
    cwd'  <- BS.packCString cwd
    h path' argv' envp' cwd' pid ppid

  voyeur_observe_exec (unVoyeurContext c) flags h' nullPtr
    
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
    path' <- BS.packCString path
    cwd' <- BS.packCString cwd
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
-- Observing processes.
--------------------------------------------------

foreign import ccall unsafe "voyeur.h voyeur_prepare"
  voyeur_prepare :: Ptr () -> Ptr CString -> IO (Ptr CString)

prepareEnvironment :: VoyeurContext -> [(String, String)] -> IO [(String, String)]
prepareEnvironment c envp = bracket newCEnvp freeCEnvp $ \envp' -> do
    cEnvp <- withArray0 nullPtr envp' (voyeur_prepare (unVoyeurContext c))
    envp'' <- peekCStringArray' cEnvp
    return $ map envSplit envp''
  where
    newCEnvp = mapM (newCString . envJoin) envp
    freeCEnvp = mapM free

foreign import ccall safe "voyeur.h voyeur_start"
  voyeur_start :: Ptr () -> CPid -> IO CInt

startObserving :: VoyeurContext -> ProcessID -> IO Int
startObserving c pid = fromIntegral <$> voyeur_start (unVoyeurContext c) pid


--------------------------------------------------
-- Utility functions.
--------------------------------------------------

asBit :: Int -> Bool -> Word8
asBit n True  = shiftL 1 n
asBit _ False = 0

peekCStringArray :: Ptr CString -> IO [BS.ByteString]
peekCStringArray array = do
  array' <- peekArray0 nullPtr array
  mapM BS.packCString array'

peekCStringArray' :: Ptr CString -> IO [String]
peekCStringArray' array = do
  array' <- peekArray0 nullPtr array
  mapM peekCString array'

envJoin :: (String, String) -> String
envJoin (k, v) = concat [k, "=", v]

envSplit :: String -> (String, String)
envSplit = safeTailSnd . break (== '=')
  
safeTailSnd :: (String, String) -> (String, String)
safeTailSnd (a, b) | null b    = (a, b)
                   | otherwise = (a, tail b)
