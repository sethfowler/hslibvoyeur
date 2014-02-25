import Data.Maybe (fromJust)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Install
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)
import System.Cmd (system)
import System.Directory
import System.FilePath

main = defaultMainWithHooks simpleUserHooks
    { confHook = customConfHook
    , cleanHook = customCleanHook
    , buildHook = customBuildHook
    }

customConfHook (pkg, pbi) flags = do
  lbi <- confHook simpleUserHooks (pkg, pbi) flags

  -- Compute some paths that need to be absolute.
  curDir <- getCurrentDirectory
  let includeDir = curDir </> "libvoyeur" </> "include"
  let libDir = curDir </> "libvoyeur" </> "build"

  let lpd   = localPkgDescr lbi
  let lib   = fromJust (library lpd)
  let libbi = libBuildInfo lib

  let libbi' = libbi
               { extraLibDirs = extraLibDirs libbi ++ [libDir]
               , includeDirs  = includeDirs  libbi ++ [".", includeDir]
               , ldOptions    = ["-lvoyeur"]       ++
                                ldOptions    libbi
               }

  let lib' = lib { libBuildInfo = libbi' }
  let lpd' = lpd { library = Just lib' }

  return $ lbi { localPkgDescr = lpd' }

customBuildHook pkg lbi flags ppHandlers = do
    putStrLn "Building libvoyeur..."
    system $ "cd libvoyeur && make"

    (buildHook simpleUserHooks) pkg lbi flags ppHandlers

customCleanHook pkg v hooks flags = do
    putStrLn "Cleaning libvoyeur..."
    system $ "cd libvoyeur && make clean"

    (cleanHook simpleUserHooks) pkg v hooks flags
