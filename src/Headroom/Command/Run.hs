{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Command.Run
-- Description : Handler for the @run@ command.
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Module representing the @run@ command, the core command of /Headroom/, which is
-- responsible for license header management.
module Headroom.Command.Run
    ( CommandRunError (..)
    , commandRun
    , loadTemplateRefs
    , loadTemplates
    , typeOfTemplate
    , writeSourceFile

      -- * License Header Post-processing
    , postProcessHeader'
    )
where

import Control.Monad.Extra (ifM)
import Data.String.Interpolate
    ( i
    , iii
    )
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.VCS.Ignore
    ( Git
    , Repo (..)
    , findRepo
    )
import Headroom.Command.Bootstrap
    ( bootstrap
    , globalKVStore
    , runRIO'
    )
import Headroom.Command.Run.Config
    ( currentYear
    , finalConfiguration
    )
import Headroom.Command.Run.Write
    ( CommandRunError (..)
    , writeSourceFile
    )
import Headroom.Command.Types (CommandRunOptions (..))
import Headroom.Config.Types
    ( AppConfig (..)
    , CtAppConfig
    , CtPostProcessConfigs
    , HeaderConfig (..)
    , HeaderSyntax (..)
    , RunMode (..)
    )
import Headroom.Data.EnumExtra (EnumExtra (..))
import Headroom.Data.Has
    ( Has (..)
    , HasRIO
    )
import Headroom.Data.Lens
    ( suffixLenses
    , suffixLensesFor
    )
import Headroom.Embedded (licenseTemplate)
import Headroom.FileSupport
    ( analyzeSourceCode
    , fileSupport
    )
import Headroom.FileType (fileTypeByExt)
import Headroom.FileType.Types (FileType (..))
import Headroom.Header
    ( addHeader
    , dropHeader
    , extractHeaderInfo
    , extractHeaderTemplate
    , replaceHeader
    )
import Headroom.Header.Sanitize (sanitizeSyntax)
import Headroom.Header.Types
    ( HeaderInfo (..)
    , HeaderTemplate (..)
    )
import Headroom.IO.FileSystem
    ( FileSystem (..)
    , excludePaths
    , fileExtension
    , mkFileSystem
    )
import Headroom.IO.KVStore (KVStore)
import Headroom.IO.Network
    ( Network (..)
    , mkNetwork
    )
import Headroom.Meta
    ( TemplateType
    )
import Headroom.PostProcess
    ( mkConfiguredEnv
    , postProcessHeader
    )
import Headroom.SourceCode
    ( SourceCode
    , toText
    )
import Headroom.Template (Template (..))
import Headroom.Template.TemplateRef
    ( TemplateRef (..)
    , renderRef
    )
import Headroom.Types (CurrentYear (..))
import Headroom.UI
    ( Progress (..)
    , zipWithProgress
    )
import Headroom.UI.Table (Table2 (..))
import Headroom.Variables
    ( compileVariables
    , dynamicVariables
    )
import Headroom.Variables.Types (Variables (..))
import RIO
import RIO.FilePath (takeBaseName)
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.Text as T

suffixLensesFor ["acPostProcessConfigs"] ''AppConfig

-- | Action to be performed based on the selected 'RunMode'.
data RunAction = RunAction
    { raProcessed :: Bool
    , raFunc :: SourceCode -> SourceCode
    , raProcessedMsg :: Text
    , raSkippedMsg :: Text
    }

-- | Full /RIO/ environment for the /Run/ command.
data Env = Env
    { envLogFunc :: LogFunc
    , envRunOptions :: CommandRunOptions
    , envConfiguration :: ~CtAppConfig
    , envCurrentYear :: CurrentYear
    , envKVStore :: ~(KVStore (RIO Env))
    , envNetwork :: Network (RIO Env)
    , envFileSystem :: FileSystem (RIO Env)
    }

suffixLenses ''Env

instance Has CtAppConfig Env where
    hasLens = envConfigurationL

instance Has CtPostProcessConfigs Env where
    hasLens = envConfigurationL . acPostProcessConfigsL

instance HasLogFunc Env where
    logFuncL = envLogFuncL

instance Has CommandRunOptions Env where
    hasLens = envRunOptionsL

instance Has CurrentYear Env where
    hasLens = envCurrentYearL

instance Has (Network (RIO Env)) Env where
    hasLens = envNetworkL

instance Has (FileSystem (RIO Env)) Env where
    hasLens = envFileSystemL

instance Has (KVStore (RIO Env)) Env where
    hasLens = envKVStoreL

getEnv :: CommandRunOptions -> LogFunc -> IO Env
getEnv opts logFunc = do
    currentYear' <- currentYear
    let env0 =
            Env
                { envLogFunc = logFunc
                , envRunOptions = opts
                , envConfiguration = undefined
                , envCurrentYear = currentYear'
                , envKVStore = undefined
                , envNetwork = mkNetwork
                , envFileSystem = mkFileSystem
                }
    config <- runRIO env0 finalConfiguration
    kvStore <- runRIO env0 globalKVStore
    pure env0{envConfiguration = config, envKVStore = kvStore}

commandRun
    :: CommandRunOptions
    -- ^ /Run/ command options
    -> IO ()
    -- ^ execution result
commandRun opts = runRIO' (getEnv opts) (croDebug opts) $ do
    _ <- bootstrap
    CommandRunOptions{..} <- viewL
    AppConfig{..} <- viewL @CtAppConfig
    let isCheck = acRunMode == Check
    warnOnDryRun
    startTS <- liftIO getPOSIXTime
    templates <- loadTemplates
    sourceFiles <- findSourceFiles (M.keys templates)
    _ <- logInfo "-----"
    (total, processed) <- processSourceFiles @TemplateType templates sourceFiles
    endTS <- liftIO getPOSIXTime
    when (processed > 0) $ logStickyDone "-----"
    logStickyDone
        $ mconcat
            [ "Done: "
            , if isCheck then "outdated " else "modified "
            , display processed
            , if isCheck then ", up-to-date " else ", skipped "
            , display (total - processed)
            , " files in "
            , displayShow (endTS - startTS)
            , " seconds."
            ]
    warnOnDryRun
    when (not croDryRun && isCheck && processed > 0) (exitWith $ ExitFailure 1)

warnOnDryRun :: (HasLogFunc env, Has CommandRunOptions env) => RIO env ()
warnOnDryRun = do
    CommandRunOptions{..} <- viewL
    when croDryRun $ logWarn "[!] Running with '--dry-run', no files are changed!"

findSourceFiles
    :: (Has CtAppConfig env, HasRIO FileSystem env, HasLogFunc env)
    => [FileType]
    -> RIO env [FilePath]
findSourceFiles fileTypes = do
    AppConfig{..} <- viewL
    FileSystem{..} <- viewL
    logDebug $ "Using source paths: " <> displayShow acSourcePaths
    files <-
        mconcat
            <$> mapM (fsFindFilesByTypes acLicenseHeaders fileTypes) acSourcePaths
    notIgnored <- excludePaths acExcludedPaths <$> excludeIgnored files
    logInfo
        [iii|
      Found #{length notIgnored} files to process
      (excluded #{length files - length notIgnored})
    |]
    pure notIgnored

excludeIgnored
    :: (Has CtAppConfig env, HasRIO FileSystem env, HasLogFunc env)
    => [FilePath]
    -> RIO env [FilePath]
excludeIgnored paths = do
    AppConfig{..} <- viewL @CtAppConfig
    FileSystem{..} <- viewL
    currentDir <- fsGetCurrentDirectory
    maybeRepo <-
        ifM
            (pure acExcludeIgnoredPaths)
            (findRepo' currentDir)
            (pure Nothing)
    case maybeRepo of
        Just repo -> filterM (fmap not . isIgnored repo) paths
        Nothing -> pure paths
  where
    findRepo' dir = do
        logInfo "Searching for VCS repository to extract exclude patterns from..."
        maybeRepo <- findRepo @_ @Git dir
        case maybeRepo of
            Just r -> logInfo [i|Found #{repoName r} repository in: #{dir}|]
            _ -> logInfo [i|No VCS repository found in: #{dir}|]
        pure maybeRepo

processSourceFiles
    :: forall a env
     . ( Template a
       , Has CtAppConfig env
       , Has CtPostProcessConfigs env
       , Has CommandRunOptions env
       , Has CurrentYear env
       , HasLogFunc env
       , HasRIO FileSystem env
       )
    => Map FileType HeaderTemplate
    -> [FilePath]
    -> RIO env (Int, Int)
processSourceFiles templates paths = do
    AppConfig{..} <- viewL
    year <- viewL
    let dVars = dynamicVariables year
        withTemplate = mapMaybe (template acLicenseHeaders) paths
    cVars <- compileVariables @a (dVars <> acVariables)
    processed <- mapM (process cVars dVars) (zipWithProgress withTemplate)
    pure (length withTemplate, length . filter id $ processed)
  where
    fileType c p = fileExtension p >>= fileTypeByExt c
    template c p = (,p) <$> (fileType c p >>= \ft -> M.lookup ft templates)
    process cVars dVars (pr, (ht, p)) = processSourceFile @a cVars dVars pr ht p

processSourceFile
    :: forall a env
     . ( Template a
       , Has CommandRunOptions env
       , Has CtAppConfig env
       , Has CtPostProcessConfigs env
       , Has CurrentYear env
       , HasLogFunc env
       , HasRIO FileSystem env
       )
    => Variables
    -> Variables
    -> Progress
    -> HeaderTemplate
    -> FilePath
    -> RIO env Bool
processSourceFile cVars dVars progress ht@HeaderTemplate{..} path = do
    AppConfig{..} <- viewL @CtAppConfig
    CommandRunOptions{..} <- viewL
    FileSystem{..} <- viewL
    fileContent <- fsLoadFile path
    let fs = fileSupport htFileType
        source = analyzeSourceCode fs fileContent
        headerInfo@HeaderInfo{..} = extractHeaderInfo ht source
        variables = dVars <> cVars <> hiVariables
        syntax = hcHeaderSyntax hiHeaderConfig
    header' <- renderTemplate variables htTemplate
    header <- postProcessHeader' @a syntax variables header'
    RunAction{..} <- chooseAction headerInfo header
    let result = raFunc source
        changed = raProcessed && (source /= result)
        message = if changed then raProcessedMsg else raSkippedMsg
        logFn = if changed then logInfo else logSticky
    logDebug $ "Header info: " <> displayShow headerInfo
    logFn $ mconcat [display progress, " ", display message, fromString path]
    writeSourceFile
        croDryRun
        acRunMode
        changed
        path
        fileContent
        (toText result)
    pure changed

chooseAction :: (Has CtAppConfig env) => HeaderInfo -> Text -> RIO env RunAction
chooseAction info header = do
    AppConfig{..} <- viewL @CtAppConfig
    let hasHeader = isJust $ hiHeaderPos info
    pure $ go acRunMode hasHeader
  where
    go runMode hasHeader = case runMode of
        Add -> aAction hasHeader
        Check -> cAction hasHeader
        Drop -> dAction hasHeader
        Replace -> rAction hasHeader
    aAction hasHeader =
        RunAction
            (not hasHeader)
            (addHeader info header)
            (justify "Adding header to:")
            (justify "Header already exists in:")
    cAction hasHeader =
        (rAction hasHeader)
            { raProcessedMsg = justify "Outdated header found in:"
            , raSkippedMsg = justify "Header up-to-date in:"
            }
    dAction hasHeader =
        RunAction
            hasHeader
            (dropHeader info)
            (justify "Dropping header from:")
            (justify "No header exists in:")
    rAction hasHeader = if hasHeader then rAction' else go Add hasHeader
    rAction' =
        RunAction
            True
            (replaceHeader info header)
            (justify "Replacing header in:")
            (justify "Header up-to-date in:")
    justify = T.justifyLeft 30 ' '

-- | Loads templates using given template references. If multiple sources define
-- template for the same 'FileType', then the preferred one (based on ordering
-- of 'TemplateRef' is selected).
loadTemplateRefs
    :: forall a env
     . ( Template a
       , HasRIO Network env
       , HasRIO FileSystem env
       , HasLogFunc env
       )
    => [TemplateRef]
    -- ^ template references
    -> RIO env (Map FileType a)
    -- ^ map of templates
loadTemplateRefs refs = do
    fileSystem <- viewL
    network <- viewL
    allRefs <- concat <$> mapM (getAllRefs fileSystem) refs
    refsWTp <- (\rs -> [(ft, ref) | (Just ft, ref) <- rs]) <$> zipRs allRefs
    refsWCtn <- mapM (loadContent fileSystem network) (filterPreferred refsWTp)
    M.fromList <$> mapM loadTemplate refsWCtn
  where
    zipRs rs = fmap (`zip` rs) . mapM getFileType $ rs
    exts = toList $ templateExtensions @a
    getAllRefs fs ref = case ref of
        LocalTemplateRef p -> fmap LocalTemplateRef <$> fsFindFilesByExts fs p exts
        _ -> pure [ref]
    loadContent fs n (ft, ref) =
        (ft,ref,) <$> case ref of
            InlineRef content -> pure content
            LocalTemplateRef path -> fsLoadFile fs path
            UriTemplateRef uri -> decodeUtf8Lenient <$> nDownloadContent n uri
            BuiltInRef lt ft' -> pure $ licenseTemplate lt ft'
    loadTemplate (ft, ref, T.strip -> c) = (ft,) <$> parseTemplate @a ref c
    getFileType = \case
        InlineRef _ -> pure Nothing
        BuiltInRef _ ft -> pure . Just $ ft
        other -> typeOfTemplate . T.unpack . renderRef $ other
    filterPreferred =
        mapMaybe (L.headMaybe . L.sort) . L.groupBy (\x y -> fst x == fst y)

loadTemplates
    :: ( Has CtAppConfig env
       , HasRIO Network env
       , HasRIO FileSystem env
       , HasLogFunc env
       )
    => RIO env (Map FileType HeaderTemplate)
loadTemplates = do
    AppConfig{..} <- viewL @CtAppConfig
    let allRefs = builtInRefs acBuiltInTemplates <> acTemplateRefs
    templates <- loadTemplateRefs @TemplateType allRefs
    logInfo . display . stats . M.toList $ templates
    pure $ M.mapWithKey (extractHeaderTemplate acLicenseHeaders) templates
  where
    stats =
        Table2
            . fmap
                (\(ft, t) -> ([i|Using #{ft} template:|], renderRef . templateRef $ t))
    builtInRefs = \case
        Just lt -> fmap (BuiltInRef lt) $ allValues @FileType
        _ -> []

-- | Takes path to the template file and returns detected type of the template.
typeOfTemplate
    :: (HasLogFunc env)
    => FilePath
    -- ^ path to the template file
    -> RIO env (Maybe FileType)
    -- ^ detected template type
typeOfTemplate path = do
    let fileType = textToEnum . T.pack . takeBaseName $ path
    when
        (isNothing fileType)
        (logWarn $ "Skipping unrecognized template type: " <> fromString path)
    pure fileType

-- | Performs post-processing on rendered /license header/, based on given
-- configuration. Currently the main points are to:
--
--  1. sanitize possibly corrupted comment syntax ('sanitizeSyntax')
--  2. apply /post-processors/ ('postProcessHeader')
postProcessHeader'
    :: forall a env
     . ( Template a
       , Has CtPostProcessConfigs env
       , Has CurrentYear env
       )
    => HeaderSyntax
    -- ^ syntax of the license header comments
    -> Variables
    -- ^ template variables
    -> Text
    -- ^ /license header/ to post-process
    -> RIO env Text
    -- ^ post-processed /license header/
postProcessHeader' syntax vars rawHeader = do
    configs <- viewL @CtPostProcessConfigs
    year <- viewL
    cEnv <- mkConfiguredEnv @a year vars configs
    pure . sanitizeSyntax syntax . postProcessHeader cEnv $ rawHeader
