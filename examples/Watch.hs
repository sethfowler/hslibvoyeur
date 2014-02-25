import Control.Monad
import System.Environment
import System.Process (runProcess)
import System.Process.Voyeur

main :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "Usage: watch-exec [command]"
  else
    withVoyeur (doWatch args)

doWatch :: [String] -> VoyeurContext -> IO ()
doWatch args c = do
  -- Set up our handlers.
  let execFlags = defaultObserveExecFlags { observeExecCWD = True, observeExecEnv = True }
      openFlags = defaultObserveOpenFlags { observeOpenCWD = True }
  observeExec c execFlags execHandler
  observeOpen c openFlags openHandler
  observeClose c defaultObserveCloseFlags closeHandler

  -- Set up the environment.
  envp <- prepareEnvironment c =<< getEnvironment

  -- Start the child process.
  handle <- runProcess (head args) (tail args) Nothing (Just envp) Nothing Nothing Nothing

  -- Observe it!
  void $ startObserving c handle

execHandler :: ObserveExecHandler
execHandler path argv envp cwd pid ppid = do
  putStrLn $ "[EXEC] " ++ show path ++ " " ++ show argv
                       ++ " (in " ++ show cwd ++ ") (pid "
                       ++ show pid ++ ") (ppid " ++ show ppid
                       ++ ")"
  when (not . null $ envp) $ do
    putStrLn "  environment:"
    forM_ envp $ \e ->
      putStrLn $ "    " ++ show e

openHandler :: ObserveOpenHandler
openHandler path oflag mode cwd retval pid = do
  putStrLn $ "[OPEN] " ++ show path ++ " " ++ show oflag
                       ++ " " ++ show mode ++ " " ++ show retval
                       ++ " (in " ++ show cwd ++ ") (pid "
                       ++ show pid ++ ")"

closeHandler :: ObserveCloseHandler
closeHandler fd retval pid = do
  putStrLn $ "[CLOSE] " ++ show fd ++ " " ++ show retval
                       ++ " (pid " ++ show pid ++ ")"
