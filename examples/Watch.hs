import Control.Monad
import Data.Maybe
import System.Environment
import System.Process (runProcess)
import System.Process.Voyeur

main :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "Usage: voyeur-watch [command]"
  else
    withVoyeur (doWatch args)

doWatch :: [String] -> VoyeurContext -> IO ()
doWatch args c = do
  -- Set up our handlers.
  let execFlags = defaultExecFlags { observeExecCWD = True, observeExecEnv = True }
      openFlags = defaultOpenFlags { observeOpenCWD = True }
  observeExec c execFlags execHandler
  observeExit c exitHandler
  observeOpen c openFlags openHandler
  observeClose c closeHandler

  -- Set up the environment.
  envp <- prepareEnvironment c =<< getEnvironment

  when (isJust envp) $ do
    -- Start the child process.
    handle <- runProcess (head args) (tail args) Nothing envp Nothing Nothing Nothing

    -- Observe it! startObserving only returns when the child process
    -- exits, so we don't need to wait.
    void $ startObserving c handle

execHandler :: ObserveExecHandler
execHandler path argv envp cwd pid ppid = do
  putStrLn $ "[EXEC] " ++ show path ++ " " ++ show argv
                       ++ " (in " ++ show cwd ++ ")"
                       ++ " (pid " ++ show pid ++ ")"
                       ++ " (ppid " ++ show ppid ++ ")"
  unless (null envp) $ do
    putStrLn "  environment:"
    forM_ envp $ \e ->
      putStrLn $ "    " ++ show e

exitHandler :: ObserveExitHandler
exitHandler exitCode pid ppid =
  putStrLn $ "[EXIT] " ++ show exitCode
                       ++ " (pid " ++ show pid ++ ")"
                       ++ " (ppid " ++ show ppid ++ ")"

openHandler :: ObserveOpenHandler
openHandler path oflag mode cwd retval pid =
  putStrLn $ "[OPEN] " ++ show path ++ " " ++ show oflag
                       ++ " " ++ show mode ++ " " ++ show retval
                       ++ " (in " ++ show cwd ++ ")"
                       ++ " (pid " ++ show pid ++ ")"

closeHandler :: ObserveCloseHandler
closeHandler fd retval pid =
  putStrLn $ "[CLOSE] " ++ show fd ++ " " ++ show retval
                       ++ " (pid " ++ show pid ++ ")"
