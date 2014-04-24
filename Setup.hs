{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.Traversable (for)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Simple.Program
import qualified Distribution.Verbosity as Verbosity
import System.Directory
import System.FilePath
import System.Info

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { confHook  = customConfHook
    , buildHook = customBuildHook
    , copyHook  = customCopyHook
    , cleanHook = customCleanHook

    , hookedPrograms = hookedPrograms simpleUserHooks
                    ++ [ makeProgram ]
    }

customConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags
               -> IO LocalBuildInfo
customConfHook (pkg, pbi) flags = do
  (_, includeDir, _) <- libvoyeurPaths
  let addIncludeDirs = (onLocalLibBuildInfo . onIncludeDirs) (++ [".", includeDir])
      addLibs = if os == "darwin"
                  then id
                  else (onLocalLibBuildInfo . onLdOptions) (++ ["-lbsd"])

  lbi <- confHook simpleUserHooks (pkg, pbi) flags
  return $ (addLibs . addIncludeDirs) lbi

customBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
customBuildHook pkg lbi usrHooks flags = do
  putStrLn "Building libvoyeur..."
  
  (libvoyeurDir, _, libDir) <- libvoyeurPaths
  let verbosity = fromFlag (buildVerbosity flags)
      runMake = runDbProgram verbosity makeProgram (withPrograms lbi)
  
  inDir libvoyeurDir $
    runMake []
  
  buildHook simpleUserHooks pkg lbi usrHooks flags
  
  notice verbosity "Relinking libvoyeur.."
  
  let libObjs = map (libObjPath libDir) [ "voyeur"
                                        , "net"
                                        , "env"
                                        , "event"
                                        , "util"
                                        ]
  
      componentLibs = concatMap componentLibNames $ componentsConfigs lbi
      addStaticObjectFile libName objName = runAr ["r", libName, objName]
      runAr = runDbProgram verbosity arProgram (withPrograms lbi)
  
  forM_ componentLibs $ \componentLib -> do
    when (withVanillaLib lbi) $
         let libName = buildDir lbi </> mkLibName componentLib
         in mapM_ (addStaticObjectFile libName) libObjs
  
    when (withProfLib lbi) $
         let libName = buildDir lbi </> mkProfLibName componentLib
         in mapM_ (addStaticObjectFile libName) libObjs
  
    when (withSharedLib lbi) $
         let libName = buildDir lbi </> mkSharedLibName buildCompilerId componentLib
         in mapM_ (addStaticObjectFile libName) libObjs

customCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
customCopyHook pkg lbi hooks flags = do
  let verb = fromFlagOrDefault Verbosity.normal $ copyVerbosity flags

  copyHook simpleUserHooks pkg lbi hooks flags

  putStrLn "Installing libvoyeur helper libraries..."

  let helperLibs = [ "exec", "exit", "open", "close" ]
      helperLibFiles = map (("libvoyeur-" ++) . (<.> dllExtension)) helperLibs
      helperLibDir = datadir (absoluteInstallDirs pkg lbi NoCopyDest)

  (_, _, libDir) <- libvoyeurPaths
  copyFiles verb helperLibDir $ map (libDir,) helperLibFiles
      
customCleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
customCleanHook pkg v hooks flags = do
  putStrLn "Cleaning libvoyeur..."
  
  let verb = fromFlagOrDefault Verbosity.normal $ cleanVerbosity flags
  pgmConf <- configureProgram verb (simpleProgram "make") defaultProgramConfiguration
  
  (libvoyeurDir, _, _) <- libvoyeurPaths
  inDir libvoyeurDir $
    runDbProgram verb makeProgram pgmConf ["clean"]
  
  cleanHook simpleUserHooks pkg v hooks flags

libvoyeurPaths :: IO (FilePath, FilePath, FilePath)
libvoyeurPaths = do
  curDir <- getCurrentDirectory
  return (curDir </> "libvoyeur",
          curDir </> "libvoyeur" </> "include",
          curDir </> "libvoyeur" </> "build")
  
componentLibNames :: (ComponentName, ComponentLocalBuildInfo, [ComponentName]) -> [LibraryName]
componentLibNames (_, LibComponentLocalBuildInfo {..}, _) = componentLibraries
componentLibNames _                                       = []

makeProgram :: Program
makeProgram = simpleProgram "make"

libObjPath :: FilePath -> FilePath -> FilePath
libObjPath dir name = dir </> name <.> objExtension

inDir :: FilePath -> IO a -> IO a
inDir dir act = do
  curDir <- getCurrentDirectory
  bracket_ (setCurrentDirectory dir)
           (setCurrentDirectory curDir)
           act

type Lifter a b = (a -> a) -> b -> b

onLocalPkgDescr :: Lifter PackageDescription LocalBuildInfo
onLocalPkgDescr f lbi = lbi { localPkgDescr = f (localPkgDescr lbi) }

onLibrary :: Lifter Library PackageDescription
onLibrary f lpd = lpd { library = f <$> library lpd }

onLibBuildInfo :: Lifter BuildInfo Library
onLibBuildInfo f lib = lib { libBuildInfo = f (libBuildInfo lib) }

onLocalLibBuildInfo :: Lifter BuildInfo LocalBuildInfo
onLocalLibBuildInfo = onLocalPkgDescr . onLibrary . onLibBuildInfo

onIncludeDirs :: Lifter [FilePath] BuildInfo
onIncludeDirs f libbi = libbi { includeDirs = f (includeDirs libbi) }

onLdOptions :: Lifter [FilePath] BuildInfo
onLdOptions f libbi = libbi { ldOptions = f (ldOptions libbi) }

onPkgDescr :: Lifter PackageDescription GenericPackageDescription
onPkgDescr f gpd = gpd { packageDescription = f (packageDescription gpd) }

onExtraSrcFiles :: Lifter [FilePath] PackageDescription
onExtraSrcFiles f pd = pd { extraSrcFiles = f (extraSrcFiles pd) }
