{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Command.Init
-- Description : Handler for the @init@ command
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Module representing the @init@ command, responsible for generating all the
-- required files (configuration, templates) for the given project, which are then
-- required by the @run@ or @gen@ commands.
module Headroom.Command.Init
    ( CommandInitError (..)
    , Env (..)
    , Paths (..)
    , commandInit
    , doesAppConfigExist
    , findSupportedFileTypes
    , initializeProject
    )
where

import Data.String.Interpolate (iii)
import Headroom.Command.Types (CommandInitOptions (..))
import Headroom.Command.Utils (bootstrap)
import Headroom.Config
    ( makeHeadersConfig
    , parseAppConfig
    )
import Headroom.Config.Enrich
    ( Enrich (..)
    , replaceEmptyValue
    , withArray
    , withText
    )
import Headroom.Config.Types
    ( AppConfig (..)
    , LicenseType (..)
    )
import Headroom.Data.Has
    ( Has (..)
    , HasRIO
    )
import Headroom.Data.Lens (suffixLenses)
import Headroom.Embedded
    ( configFileStub
    , defaultConfig
    , licenseTemplate
    )
import Headroom.FileType (fileTypeByExt)
import Headroom.FileType.Types (FileType (..))
import Headroom.IO.FileSystem
    ( FileSystem (..)
    , fileExtension
    , findFiles
    , mkFileSystem
    )
import Headroom.Meta
    ( TemplateType
    , buildVersion
    , configFileName
    )
import Headroom.Meta.Version (printVersion)
import Headroom.Template (Template (..))
import Headroom.Types
    ( fromHeadroomError
    , toHeadroomError
    )
import RIO
import qualified RIO.Char as C
import RIO.FilePath
    ( takeDirectory
    , (</>)
    )
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

---------------------------------  DATA TYPES  ---------------------------------

-- | /RIO/ Environment for the @init@ command.
data Env = Env
    { envLogFunc :: LogFunc
    , envFileSystem :: FileSystem (RIO Env)
    , envInitOptions :: CommandInitOptions
    , envPaths :: Paths
    }

-- | Paths to various locations of file system.
data Paths = Paths
    { pConfigFile :: FilePath
    , pTemplatesDir :: FilePath
    }

-- | A file that will be created during project initialization.
data PlannedFile = PlannedFile
    { pfTargetPath :: FilePath
    , pfContent :: Text
    }

-- | A temporary file ready to be moved to its target path.
data StagedFile = StagedFile
    { sfTemporaryPath :: FilePath
    , sfTargetPath :: FilePath
    }

suffixLenses ''Env

instance HasLogFunc Env where
    logFuncL = envLogFuncL

instance Has CommandInitOptions Env where
    hasLens = envInitOptionsL

instance Has (FileSystem (RIO Env)) Env where
    hasLens = envFileSystemL

instance Has Paths Env where
    hasLens = envPathsL

env' :: CommandInitOptions -> LogFunc -> IO Env
env' opts logFunc = do
    let paths =
            Paths
                { pConfigFile = configFileName
                , pTemplatesDir = "headroom-templates"
                }
    pure
        $ Env
            { envLogFunc = logFunc
            , envFileSystem = mkFileSystem
            , envInitOptions = opts
            , envPaths = paths
            }

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Handler for @init@ command.
commandInit
    :: CommandInitOptions
    -- ^ @init@ command options
    -> IO ()
    -- ^ execution result
commandInit opts =
    bootstrap (env' opts) False
        $ findSupportedFileTypes
        >>= initializeProject

-- | Safely initializes the project for the provided file types.
initializeProject
    :: ( Has CommandInitOptions env
       , HasLogFunc env
       , HasRIO FileSystem env
       , Has Paths env
       )
    => [FileType]
    -> RIO env ()
initializeProject fileTypes = do
    paths <- viewL
    plannedFiles <- planFiles fileTypes
    ensureTargetsAvailable plannedFiles
    bracketOnError
        (stageFiles (takeDirectory $ pConfigFile paths) plannedFiles)
        (cleanupFiles . fmap sfTemporaryPath)
        $ \stagedFiles -> do
            ensureTargetsAvailable plannedFiles
            commitFiles (pTemplatesDir paths) stagedFiles
            logInfo "Project initialization completed"

-- | Recursively scans provided source paths for known file types for which
-- templates can be generated.
findSupportedFileTypes
    :: (Has CommandInitOptions env, HasLogFunc env)
    => RIO env [FileType]
findSupportedFileTypes = do
    opts <- viewL
    pHeadersConfig <- acLicenseHeaders <$> parseAppConfig defaultConfig
    headersConfig <- makeHeadersConfig pHeadersConfig
    fileTypes <- do
        allFiles <-
            mapM
                (\path -> findFiles path (const True))
                (cioSourcePaths opts)
        let allFileTypes =
                fmap
                    (fileExtension >=> fileTypeByExt headersConfig)
                    (concat allFiles)
        pure . L.nub . catMaybes $ allFileTypes
    case fileTypes of
        [] -> throwM NoProvidedSourcePaths
        _ -> do
            logInfo $ "Found supported file types: " <> displayShow fileTypes
            pure fileTypes

-- | Checks whether application config file already exists.
doesAppConfigExist
    :: (HasLogFunc env, HasRIO FileSystem env, Has Paths env)
    => RIO env Bool
doesAppConfigExist = do
    FileSystem{..} <- viewL
    Paths{..} <- viewL
    logInfo "Verifying that there's no existing Headroom configuration..."
    fsDoesFileExist pConfigFile

------------------------------  PRIVATE FUNCTIONS  -----------------------------

planFiles
    :: (Has CommandInitOptions env, Has Paths env)
    => [FileType]
    -> RIO env [PlannedFile]
planFiles fileTypes = do
    opts <- viewL
    paths <- viewL
    let templates =
            fmap (planTemplate paths . (cioLicenseType opts,)) fileTypes
    pure $ templates <> [planConfig opts paths]

planTemplate :: Paths -> (LicenseType, FileType) -> PlannedFile
planTemplate Paths{..} (licenseType, fileType) =
    let extension = NE.head $ templateExtensions @TemplateType
        file = (fmap C.toLower . show $ fileType) <> "." <> T.unpack extension
     in PlannedFile
            { pfTargetPath = pTemplatesDir </> file
            , pfContent = licenseTemplate licenseType fileType
            }

planConfig :: CommandInitOptions -> Paths -> PlannedFile
planConfig opts Paths{..} =
    PlannedFile
        { pfTargetPath = pConfigFile
        , pfContent = enrich (modify opts) configFileStub
        }
  where
    modify options =
        mconcat
            [ replaceEmptyValue "version" $ withText (printVersion buildVersion)
            , replaceEmptyValue "source-paths" $ withArray (cioSourcePaths options)
            , replaceEmptyValue "template-paths" $ withArray [pTemplatesDir]
            ]

ensureTargetsAvailable
    :: (HasRIO FileSystem env)
    => [PlannedFile]
    -> RIO env ()
ensureTargetsAvailable plannedFiles = do
    FileSystem{..} <- viewL
    existingPaths <-
        filterM
            (\path -> (||) <$> fsDoesFileExist path <*> fsDoesDirectoryExist path)
            $ pfTargetPath
            <$> plannedFiles
    case existingPaths of
        [] -> pure ()
        path : paths ->
            throwM . InitializationFileAlreadyExists $ path :| paths

stageFiles
    :: (HasLogFunc env, HasRIO FileSystem env)
    => FilePath
    -> [PlannedFile]
    -> RIO env [StagedFile]
stageFiles _ [] = pure []
stageFiles stagingDirectory (PlannedFile{..} : remainingFiles) = do
    FileSystem{..} <- viewL
    logInfo $ "Staging initialization file for " <> fromString pfTargetPath
    bracketOnError
        (fsWriteTempFile stagingDirectory pfContent)
        (cleanupFiles . pure)
        $ \temporaryPath -> do
            remainingStaged <- stageFiles stagingDirectory remainingFiles
            pure $ StagedFile temporaryPath pfTargetPath : remainingStaged

commitFiles
    :: (HasLogFunc env, HasRIO FileSystem env)
    => FilePath
    -> [StagedFile]
    -> RIO env ()
commitFiles templatesDirectory stagedFiles = do
    mask_ $ do
        FileSystem{..} <- viewL
        directoryExisted <- fsDoesDirectoryExist templatesDirectory
        fsCreateDirectory templatesDirectory
            `onException` cleanupDirectory directoryExisted templatesDirectory
        moveFiles directoryExisted [] stagedFiles
  where
    moveFiles _ _ [] = pure ()
    moveFiles
        directoryExisted
        committedPaths
        remaining@(stagedFile : rest) = do
            FileSystem{..} <- viewL
            let temporaryPath = sfTemporaryPath stagedFile
                targetPath = sfTargetPath stagedFile
            logInfo $ "Creating initialization file in " <> fromString targetPath
            result <- tryAny $ fsRenameFile temporaryPath targetPath
            case result of
                Right () ->
                    moveFiles directoryExisted (targetPath : committedPaths) rest
                Left err -> do
                    cleanupFiles committedPaths
                    cleanupFiles $ fmap sfTemporaryPath remaining
                    cleanupDirectory directoryExisted templatesDirectory
                    throwM err

cleanupFiles :: (HasRIO FileSystem env) => [FilePath] -> RIO env ()
cleanupFiles paths = do
    FileSystem{..} <- viewL
    forM_ paths $ \path -> void . tryAny $ fsRemoveFile path

cleanupDirectory
    :: (HasRIO FileSystem env)
    => Bool
    -> FilePath
    -> RIO env ()
cleanupDirectory existed path = unless existed $ do
    FileSystem{..} <- viewL
    void . tryAny $ fsRemoveDirectory path

---------------------------------  ERROR TYPES  --------------------------------

-- | Exception specific to the "Headroom.Command.Init" module
data CommandInitError
    = -- | one or more initialization target files already exist
      InitializationFileAlreadyExists (NonEmpty FilePath)
    | -- | no paths to source code files provided
      NoProvidedSourcePaths
    | -- | no supported file types found on source paths
      NoSupportedFileType
    deriving (Eq, Show)

instance Exception CommandInitError where
    displayException = displayException'
    toException = toHeadroomError
    fromException = fromHeadroomError

displayException' :: CommandInitError -> String
displayException' = \case
    InitializationFileAlreadyExists paths ->
        [iii|
      Cannot initialize project because these files already exist:
      #{T.unlines $ T.pack . ("- " <>) <$> NE.toList paths}
    |]
    NoProvidedSourcePaths ->
        [iii|
      No source code paths (files or directories) defined
    |]
    NoSupportedFileType ->
        [iii|
      No supported file type found in scanned source paths
    |]
