import Control.Monad
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
  let execFlags = defaultObserveExecFlags { observeExecCWD = True, observeExecEnv = True }
      openFlags = defaultObserveOpenFlags { observeOpenCWD = True }
      closeFlags = defaultObserveCloseFlags
  observeExec c execFlags execHandler
  observeOpen c openFlags openHandler
  observeClose c closeFlags closeHandler

  -- Set up the environment.
  envp <- prepareEnvironment c =<< getEnvironment

  -- Start the child process.
  handle <- runProcess (head args) (tail args) Nothing (Just envp) Nothing Nothing Nothing

  -- Observe it! startObserving only returns when the child process
  -- exits, so we don't need to wait.
  void $ startObserving c handle

execHandler :: ObserveExecHandler
execHandler path argv envp cwd pid ppid = do
  putStrLn $ "[EXEC] " ++ show path ++ " " ++ show argv
                       ++ " (in " ++ show cwd ++ ") (pid "
                       ++ show pid ++ ") (ppid " ++ show ppid
                       ++ ")"
  unless (null envp) $ do
    putStrLn "  environment:"
    forM_ envp $ \e ->
      putStrLn $ "    " ++ show e

openHandler :: ObserveOpenHandler
openHandler path oflag mode cwd retval pid =
  putStrLn $ "[OPEN] " ++ show path ++ " " ++ show oflag
                       ++ " " ++ show mode ++ " " ++ show retval
                       ++ " (in " ++ show cwd ++ ") (pid "
                       ++ show pid ++ ")"

closeHandler :: ObserveCloseHandler
closeHandler fd retval pid =
  putStrLn $ "[CLOSE] " ++ show fd ++ " " ++ show retval
                       ++ " (pid " ++ show pid ++ ")"
