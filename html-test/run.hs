#!/usr/bin/env runhaskell
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}


import Control.Applicative
import Control.Monad

import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Maybe
import Data.List

import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Simple.GHC
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Verbosity

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

import qualified Text.XML.Light as Xml


baseDir, rootDir :: FilePath
baseDir = takeDirectory __FILE__
rootDir = baseDir </> ".."

srcDir, refDir, outDir :: FilePath
srcDir = baseDir </> "src"
refDir = baseDir </> "ref"
outDir = baseDir </> "out"

resDir :: FilePath
resDir = rootDir </> "resources"


data Config = Config
    { cfgHaddockPath :: FilePath
    , cfgGhcPath :: FilePath
    , cfgFiles :: [FilePath]
    , cfgHaddockArgs :: [String]
    , cfgHaddockStdOut :: FilePath
    , cfgDiffTool :: Maybe FilePath
    , cfgEnv :: Environment
    }


data CheckResult
    = Fail
    | Pass
    | NoRef


main :: IO ()
main = do
    cfg <- uncurry loadConfig =<< checkOpt =<< getArgs
    runHaddock cfg
    checkFiles cfg


checkFiles :: Config -> IO ()
checkFiles (Config { .. }) = do
    putStrLn "Testing output files..."
    failed <- liftM catMaybes . forM cfgFiles $ \file -> do
        let mdl = takeBaseName file
        putStr $ "Checking " ++ mdl ++ "... "

        status <- checkModule mdl
        case status of
            Fail -> putStrLn "FAIL" >> (return $ Just mdl)
            Pass -> putStrLn "PASS" >> (return Nothing)
            NoRef -> putStrLn "PASS [no .ref]" >> (return Nothing)

    if null failed
        then do
            putStrLn "All tests passed!"
            exitSuccess
        else do
            maybeDiff cfgDiffTool failed
            exitFailure


maybeDiff :: Maybe FilePath -> [String] -> IO ()
maybeDiff Nothing _ = pure ()
maybeDiff (Just diff) mdls = do
    putStrLn "Diffing failed cases..."
    forM_ mdls $ diffModule diff


runHaddock :: Config -> IO ()
runHaddock (Config { .. }) = do
    putStrLn "Running Haddock process..."

    haddockStdOut <- openFile cfgHaddockStdOut WriteMode
    handle <- runProcess' cfgHaddockPath $ processConfig
        { pcArgs = cfgHaddockArgs ++ cfgFiles
        , pcEnv = Just $ cfgEnv
        , pcStdOut = Just $ haddockStdOut
        }
    waitForSuccess "Failed to run Haddock on specified test files" handle


checkOpt :: [String] -> IO ([Flag], [String])
checkOpt args = do
    let (flags, files, errors) = getOpt Permute options args

    unless (null errors) $ do
        hPutStr stderr $ concat errors
        exitFailure

    when (FlagHelp `elem` flags) $ do
        hPutStrLn stderr $ usageInfo "" options
        exitSuccess

    return (flags, files)


loadConfig :: [Flag] -> [String] -> IO Config
loadConfig flags files = do
    cfgEnv <- (:) ("haddock_datadir", resDir) <$> getEnvironment

    cfgHaddockPath <- pure $ flip fromMaybe (flagsHaddockPath flags) $
        rootDir </> "dist" </> "build" </> "haddock" </> "haddock"

    printVersions cfgEnv cfgHaddockPath

    cfgGhcPath <- flip fromMaybe (flagsGhcPath flags) <$>
         init <$> rawSystemStdout normal cfgHaddockPath ["--print-ghc-path"]

    cfgFiles <- processFileArgs files

    cfgHaddockArgs <- liftM concat . sequence $
        [ pure ["--no-warnings"]
        , pure ["--odir=" ++ outDir]
        , pure ["--pretty-html"]
        , pure ["--html"]
        , pure ["--optghc=-w"]
        , pure $ flagsHaddockOptions flags
        , baseDependencies cfgGhcPath
        ]

    let cfgHaddockStdOut = fromMaybe "/dev/null" (flagsHaddockStdOut flags)

    cfgDiffTool <- (<|>) <$> pure (flagsDiffTool flags) <*> defaultDiffTool

    return $ Config { .. }


checkModule :: String -> IO CheckResult
checkModule mdl = do
    hasRef <- doesFileExist $ refFile mdl
    if hasRef
        then do
            Just outXml <- readXml $ outFile mdl
            Just refXml <- readXml $ refFile mdl
            return $ if strip outXml == strip refXml
                then Pass
                else Fail
        else return NoRef


diffModule :: FilePath -> String -> IO ()
diffModule diff mdl = do
    Just outXml <- readXml $ outFile mdl
    Just refXml <- readXml $ refFile mdl
    let outXml' = strip outXml
    let refXml' = strip refXml
    writeFile outFile' $ Xml.ppElement outXml'
    writeFile refFile' $ Xml.ppElement refXml'

    putStrLn $ "Diff for module " ++ show mdl ++ ":"
    handle <- runProcess' diff $ processConfig
        { pcArgs = [outFile', refFile']
        }
    waitForProcess handle >> return ()
  where
    outFile' = outFile mdl <.> "nolinks"
    refFile' = outFile mdl <.> "ref" <.> "nolinks"


outFile :: String -> FilePath
outFile mdl = outDir </> mdl <.> "html"


refFile :: String -> FilePath
refFile mdl = refDir </> mdl <.> "html"


printVersions :: Environment -> FilePath -> IO ()
printVersions env haddockPath = do
    handle <- runProcess' haddockPath $ processConfig
        { pcEnv = Just env
        , pcArgs = ["--version"]
        }
    waitForSuccess "Failed to run `haddock --version`" handle

    handle <- runProcess' haddockPath $ processConfig
        { pcEnv = Just env
        , pcArgs = ["--ghc-version"]
        }
    waitForSuccess "Failed to run `haddock --ghc-version`" handle


baseDependencies :: FilePath -> IO [String]
baseDependencies ghcPath = do
    (_, _, cfg) <- configure normal (Just ghcPath) Nothing
        defaultProgramConfiguration
    pkgIndex <- getInstalledPackages normal [GlobalPackageDB] cfg
    mapM (getDependency pkgIndex) ["base", "process", "ghc-prim"]
  where
    getDependency pkgIndex name = case ifaces pkgIndex name of
        [] -> do
            hPutStrLn stderr $ "Couldn't find base test dependency: " ++ name
            exitFailure
        (ifArg:_) -> pure ifArg
    ifaces pkgIndex name = do
        pkg <- join $ snd <$> lookupPackageName pkgIndex (PackageName name)
        iface <$> haddockInterfaces pkg <*> haddockHTMLs pkg
    iface file html = "--read-interface=" ++ html ++ "," ++ file


defaultDiffTool :: IO (Maybe FilePath)
defaultDiffTool =
    liftM listToMaybe . filterM isAvailable $ ["colordiff", "diff"]
  where
    isAvailable = liftM isJust . findProgramLocation silent


processFileArgs :: [String] -> IO [FilePath]
processFileArgs [] =
    map toModulePath . filter isSourceFile <$> getDirectoryContents srcDir
  where
    toModulePath = modulePath . takeBaseName
processFileArgs args = pure $ map processFileArg args


processFileArg :: String -> FilePath
processFileArg arg
    | isSourceFile arg = arg
    | otherwise = modulePath arg


isSourceFile :: FilePath -> Bool
isSourceFile path = takeExtension path `elem` [".hs", ".lhs"]

modulePath :: String -> FilePath
modulePath mdl = srcDir </> mdl <.> "hs"


deriving instance Eq Xml.Content
deriving instance Eq Xml.Element
deriving instance Eq Xml.CData


readXml :: FilePath -> IO (Maybe Xml.Element)
readXml = liftM Xml.parseXMLDoc . readFile


strip :: Xml.Element -> Xml.Element
strip = stripFooter . stripLinks


stripLinks :: Xml.Element -> Xml.Element
stripLinks =
    everywhere (mkT unlink)
  where
    unlink attr@(Xml.Attr { attrKey = key })
        | Xml.qName key == "href" = attr { Xml.attrVal = "#" }
        | otherwise = attr


stripFooter :: Xml.Element -> Xml.Element
stripFooter =
    everywhere (mkT defoot)
  where
    defoot elem
        | isFooter elem = elem { Xml.elContent = [] }
        | otherwise = elem
    isFooter elem = any isFooterAttr $ Xml.elAttribs elem
    isFooterAttr (Xml.Attr { .. }) = and
        [ Xml.qName attrKey == "id"
        , attrVal == "footer"
        ]


data Flag
    = FlagHaddockPath FilePath
    | FlagGhcPath FilePath
    | FlagHaddockOptions String
    | FlagHaddockStdOut FilePath
    | FlagDiffTool FilePath
    | FlagHelp
    deriving Eq


options :: [OptDescr Flag]
options =
    [ Option [] ["haddock-path"] (ReqArg FlagHaddockPath "FILE")
        "path to Haddock executable to exectue tests with"
    , Option [] ["ghc-path"] (ReqArg FlagGhcPath "FILE")
        "path to GHC executable"
    , Option [] ["haddock-options"] (ReqArg FlagHaddockOptions "OPTS")
        "additional options to run Haddock with"
    , Option [] ["haddock-stdout"] (ReqArg FlagHaddockStdOut "FILE")
        "where to redirect Haddock output"
    , Option [] ["diff-tool"] (ReqArg FlagDiffTool "PATH")
        "diff tool to use when printing failed cases"
    , Option ['h'] ["help"] (NoArg FlagHelp)
        "display this help end exit"
    ]


flagsHaddockPath :: [Flag] -> Maybe FilePath
flagsHaddockPath flags = mlast [ path | FlagHaddockPath path <- flags ]


flagsGhcPath :: [Flag] -> Maybe FilePath
flagsGhcPath flags = mlast [ path | FlagGhcPath path <- flags ]


flagsHaddockOptions :: [Flag] -> [String]
flagsHaddockOptions flags = concat
    [ words opts | FlagHaddockOptions opts <- flags ]


flagsHaddockStdOut :: [Flag] -> Maybe FilePath
flagsHaddockStdOut flags = mlast [ path | FlagHaddockStdOut path <- flags ]


flagsDiffTool :: [Flag] -> Maybe FilePath
flagsDiffTool flags = mlast [ path | FlagDiffTool path <- flags ]


type Environment = [(String, String)]

data ProcessConfig = ProcessConfig
    { pcArgs :: [String]
    , pcWorkDir :: Maybe FilePath
    , pcEnv :: Maybe Environment
    , pcStdIn :: Maybe Handle
    , pcStdOut :: Maybe Handle
    , pcStdErr :: Maybe Handle
    }


processConfig :: ProcessConfig
processConfig = ProcessConfig
    { pcArgs = []
    , pcWorkDir = Nothing
    , pcEnv = Nothing
    , pcStdIn = Nothing
    , pcStdOut = Nothing
    , pcStdErr = Nothing
    }


runProcess' :: FilePath -> ProcessConfig -> IO ProcessHandle
runProcess' path (ProcessConfig { .. }) = runProcess
    path pcArgs pcWorkDir pcEnv pcStdIn pcStdOut pcStdErr


waitForSuccess :: String -> ProcessHandle -> IO ()
waitForSuccess msg handle = do
    result <- waitForProcess handle
    unless (result == ExitSuccess) $ do
        hPutStrLn stderr $ msg
        exitFailure


mlast :: [a] -> Maybe a
mlast = listToMaybe . reverse


-- *** OLD TEST RUNNER UTILITY FUNCTIONS ***
-- These are considered bad and should be replaced as soon as possible.


-- | List of modules in which we don't 'stripLinks'
preserveLinksModules :: [String]
preserveLinksModules = ["Bug253"]
