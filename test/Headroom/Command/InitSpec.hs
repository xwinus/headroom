{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Command.InitSpec
    ( spec
    )
where

import Headroom.Command.Init
import Headroom.Command.Types (CommandInitOptions (..))
import Headroom.Config.Types (LicenseType (..))
import Headroom.Data.Has (Has (..))
import Headroom.Data.Lens
    ( suffixLenses
    , suffixLensesFor
    )
import Headroom.Embedded (licenseTemplate)
import Headroom.FileType.Types (FileType (..))
import Headroom.IO.FileSystem
    ( FileSystem (..)
    , mkFileSystem
    )
import RIO
import qualified RIO.Directory as D
import RIO.FilePath ((</>))
import qualified RIO.List as L
import qualified RIO.Text as T
import System.IO.Error (userError)
import Test.Hspec

data TestEnv = TestEnv
    { envLogFunc :: LogFunc
    , envFileSystem :: FileSystem (RIO TestEnv)
    , envInitOptions :: CommandInitOptions
    , envPaths :: Paths
    }

suffixLenses ''TestEnv
suffixLensesFor ["fsDoesFileExist"] ''FileSystem
suffixLensesFor ["pConfigFile"] ''Paths

instance HasLogFunc TestEnv where
    logFuncL = envLogFuncL

instance Has CommandInitOptions TestEnv where
    hasLens = envInitOptionsL

instance Has (FileSystem (RIO TestEnv)) TestEnv where
    hasLens = envFileSystemL

instance Has Paths TestEnv where
    hasLens = envPathsL

spec :: Spec
spec = do
    describe "doesAppConfigExist" $ do
        it "checks that configuration file exists in selected directory" $ do
            let env' = env & envFileSystemL . fsDoesFileExistL .~ check
                check path = pure $ env' ^. envPathsL . pConfigFileL == path
            runRIO env' doesAppConfigExist `shouldReturn` True

    describe "findSupportedFileTypes" $ do
        it "recursively finds all known file types present in given path" $ do
            L.sort <$> runRIO env findSupportedFileTypes `shouldReturn` [HTML, XML]

    describe "initializeProject" $ do
        it "creates all templates and the configuration for a clean project"
            . withSystemTempDirectory "headroom-init"
            $ \directory -> do
                let testEnv = envFor directory mkFileSystem
                    haskellTemplate = directory </> "headroom-templates" </> "haskell.mustache"
                    xmlTemplate = directory </> "headroom-templates" </> "xml.mustache"
                    configFile = directory </> ".headroom.yaml"

                runRIO testEnv $ initializeProject [Haskell, XML]

                readFileUtf8 haskellTemplate
                    `shouldReturn` licenseTemplate BSD3 Haskell
                readFileUtf8 xmlTemplate
                    `shouldReturn` licenseTemplate BSD3 XML
                config <- readFileUtf8 configFile
                config `shouldSatisfy` T.isInfixOf "headroom-templates"

        it "preserves every target when a template already exists"
            . withSystemTempDirectory "headroom-init"
            $ \directory -> do
                let testEnv = envFor directory mkFileSystem
                    templatesDirectory = directory </> "headroom-templates"
                    existingTemplate = templatesDirectory </> "haskell.mustache"
                    otherTemplate = templatesDirectory </> "xml.mustache"
                    configFile = directory </> ".headroom.yaml"
                D.createDirectory templatesDirectory
                writeFileUtf8 existingTemplate "custom template"

                runRIO testEnv (initializeProject [Haskell, XML])
                    `shouldThrow` ( \case
                                        InitializationFileAlreadyExists paths ->
                                            existingTemplate `elem` paths
                                        _ -> False
                                  )

                readFileUtf8 existingTemplate `shouldReturn` "custom template"
                D.doesFileExist otherTemplate `shouldReturn` False
                D.doesFileExist configFile `shouldReturn` False

        it "removes staged files when a later staging write fails"
            . withSystemTempDirectory "headroom-init"
            $ \directory -> do
                writeCount <- newIORef (0 :: Int)
                let fileSystem = failingSecondWrite writeCount mkFileSystem
                    testEnv = envFor directory fileSystem

                runRIO testEnv (initializeProject [Haskell, XML])
                    `shouldThrow` anyIOException

                D.listDirectory directory `shouldReturn` []

        it "rolls back committed files when a later rename fails"
            . withSystemTempDirectory "headroom-init"
            $ \directory -> do
                renameCount <- newIORef (0 :: Int)
                let fileSystem = failingSecondRename renameCount mkFileSystem
                    testEnv = envFor directory fileSystem

                runRIO testEnv (initializeProject [Haskell])
                    `shouldThrow` anyIOException

                D.listDirectory directory `shouldReturn` []

env :: TestEnv
env = TestEnv{..}
  where
    envLogFunc = mkLogFunc (\_ _ _ _ -> pure ())
    envInitOptions =
        CommandInitOptions
            { cioSourcePaths = ["test-data" </> "test-traverse"]
            , cioLicenseType = BSD3
            }
    envPaths =
        Paths
            { pConfigFile = "test-data" </> "configs" </> "full.yaml"
            , pTemplatesDir = "headroom-templates"
            }
    envFileSystem =
        FileSystem
            { fsCreateDirectory = undefined
            , fsDoesDirectoryExist = undefined
            , fsDoesFileExist = undefined
            , fsFindFiles = undefined
            , fsFindFilesByExts = undefined
            , fsFindFilesByTypes = undefined
            , fsGetCurrentDirectory = undefined
            , fsGetUserDirectory = undefined
            , fsListFiles = undefined
            , fsLoadFile = undefined
            , fsRemoveDirectory = undefined
            , fsRemoveFile = undefined
            , fsRenameFile = undefined
            , fsWriteTempFile = undefined
            , fsWriteFile = undefined
            }

envFor :: FilePath -> FileSystem (RIO TestEnv) -> TestEnv
envFor directory fileSystem =
    (env & envFileSystemL .~ fileSystem)
        & envPathsL
        .~ Paths
            { pConfigFile = directory </> ".headroom.yaml"
            , pTemplatesDir = directory </> "headroom-templates"
            }

failingSecondWrite
    :: IORef Int
    -> FileSystem (RIO TestEnv)
    -> FileSystem (RIO TestEnv)
failingSecondWrite writeCount fileSystem =
    fileSystem
        { fsWriteTempFile = \directory content -> do
            count <- atomicModifyIORef' writeCount $ \value -> (value + 1, value + 1)
            if count == 2
                then throwIO $ userError "simulated staging failure"
                else fsWriteTempFile fileSystem directory content
        }

failingSecondRename
    :: IORef Int
    -> FileSystem (RIO TestEnv)
    -> FileSystem (RIO TestEnv)
failingSecondRename renameCount fileSystem =
    fileSystem
        { fsRenameFile = \source target -> do
            count <- atomicModifyIORef' renameCount $ \value -> (value + 1, value + 1)
            if count == 2
                then throwIO $ userError "simulated rename failure"
                else fsRenameFile fileSystem source target
        }
