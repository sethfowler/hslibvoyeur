import Control.Applicative
import Control.Arrow
import Control.Exception
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Simple.Program
import Distribution.Simple.Program.Ar(createArLibArchive)
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Verbosity as Verbosity
import System.Directory
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { confHook  = customConfHook
    , cleanHook = customCleanHook
    , buildHook = customBuildHook

    , hookedPrograms = hookedPrograms simpleUserHooks
                    ++ [ makeProgram ]
    }

customConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags
               -> IO LocalBuildInfo
customConfHook (pkg, pbi) flags = do
  (_, includeDir, _) <- libvoyeurPaths
  let addIncludeDirs = (onLocalLibBuildInfo . onIncludeDirs) (++ [".", includeDir])

  lbi <- confHook simpleUserHooks (pkg, pbi) flags
  return $ addIncludeDirs lbi

customBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
customBuildHook pkg lbi usrHooks flags = do
    putStrLn "Building libvoyeur..."

    (libvoyeurDir, _, libDir) <- libvoyeurPaths
    let verbosity = fromFlag (buildVerbosity flags)
        lib       = fromJust (library . localPkgDescr $ lbi)
        bdir      = buildDir lbi
        libObjs   = map (libObjPath libDir) [ "voyeur"
                                            , "net"
                                            , "env"
                                            , "event"
                                            , "util"
                                            ]
        pdb       = withPrograms lbi
        makeProg  = fromMaybe (error "'make' not found!") (lookupKnownProgram "make" pdb)

    (makeCmd, _) <- requireProgram verbosity makeProg pdb
    inDir libvoyeurDir $
      runProgram verbosity makeCmd []

    buildHook simpleUserHooks pkg lbi usrHooks flags

    notice verbosity "Relinking libvoyeur.."

    hsObjs <- getHaskellObjects lib lbi bdir objExtension (splitObjs lbi)
    let staticObjs = libObjs ++ hsObjs
        libFile    = getLibraryName bdir lbi

    removeFile libFile
    createArLibArchive verbosity lbi libFile staticObjs

customCleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
customCleanHook pkg v hooks flags = do
    putStrLn "Cleaning libvoyeur..."

    let verb = fromFlagOrDefault Verbosity.normal $ cleanVerbosity flags
    pgmConf <- configureProgram verb (simpleProgram "make") defaultProgramConfiguration

    (libvoyeurDir, _, _) <- libvoyeurPaths
    inDir libvoyeurDir $
      runDbProgram verb makeProgram pgmConf ["clean"]

    cleanHook simpleUserHooks pkg v hooks flags

makeProgram :: Program
makeProgram = simpleProgram "make"

libvoyeurPaths :: IO (FilePath, FilePath, FilePath)
libvoyeurPaths = do
  curDir <- getCurrentDirectory
  return (curDir </> "libvoyeur",
          curDir </> "libvoyeur" </> "include",
          curDir </> "libvoyeur" </> "build")
  
libPath :: FilePath -> FilePath
libPath name = name <.> dllExtension

libObjPath :: FilePath -> FilePath -> FilePath
libObjPath dir name = dir </> name <.> objExtension

asStaticLib :: FilePath -> FilePath
asStaticLib lname = mkLibName (LibraryName lname)

inDir :: FilePath -> IO a -> IO a
inDir dir act = do
  curDir <- getCurrentDirectory
  bracket_ (setCurrentDirectory dir)
           (setCurrentDirectory curDir)
           act

getHaskellObjects :: Library -> LocalBuildInfo -> FilePath -> String -> Bool -> IO [FilePath]
getHaskellObjects lib lbi pref wanted_obj_ext allow_split_objs
  | splitObjs lbi && allow_split_objs = do
        let splitSuffix = if compilerVersion (compiler lbi) <
                             Version [6, 11] []
                          then "_split"
                          else "_" ++ wanted_obj_ext ++ "_split"
            dirs = [ pref </> (ModuleName.toFilePath x ++ splitSuffix)
                   | x <- libModules lib ]
        objss <- mapM getDirectoryContents dirs
        let objs = [ dir </> obj
                   | (objs',dir) <- zip objss dirs, obj <- objs',
                     let obj_ext = takeExtension obj,
                     '.':wanted_obj_ext == obj_ext ]
        return objs
  | otherwise  =
        return [ pref </> ModuleName.toFilePath x <.> wanted_obj_ext
               | x <- libModules lib ]

getLibraryName :: FilePath -> LocalBuildInfo -> FilePath
getLibraryName pref lbi = getLibName $ filter isLib $ componentsConfigs lbi
    where isLib (CLibName, _, _) = True
          isLib _ = False
          getLibName [(_,clbi,_)]
              | LibraryName lname <- head (componentLibraries clbi) = pref </> asStaticLib lname
          getLibName [] = error "getLibName: No libraries found!"
          getLibName _ = error "getLibName: more than one library found!"

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

onExtraLibs :: Lifter [FilePath] BuildInfo
onExtraLibs f libbi = libbi { extraLibs = f (extraLibs libbi) }

onExtraLibDirs :: Lifter [FilePath] BuildInfo
onExtraLibDirs f libbi = libbi { extraLibDirs = f (extraLibDirs libbi) }
