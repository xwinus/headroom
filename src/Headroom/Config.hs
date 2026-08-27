{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Config
-- Description : Configuration handling (loading, parsing, validating)
-- Copyright   : (c) 2019-2023 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides logic for working with the cofiguration data types.
-- Headroom uses the
-- <https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67 partial options monoid>
-- pattern for the configuration.
module Headroom.Config
    ( -- * Loading & Parsing Configuration
      loadAppConfig
    , parseAppConfig

      -- * Processing Partial Configuration
    , makeAppConfig
    , makeHeadersConfig
    , makeHeaderConfig
    )
where

import Data.Monoid (Last (..))
import Headroom.Config.Compat (checkCompatibility)
import Headroom.Config.Parse
    ( ConfigurationScope (ProjectConfiguration)
    , decodeConfiguration
    )
import Headroom.Config.Types
    ( AppConfig (..)
    , ConfigurationError (..)
    , ConfigurationKey (..)
    , CtAppConfig
    , CtHeaderConfig
    , CtHeadersConfig
    , CtPostProcessConfig
    , CtPostProcessConfigs
    , CtUpdateCopyrightConfig
    , HeaderConfig (..)
    , HeadersConfig (..)
    , Phase (..)
    , PostProcessConfig (..)
    , PostProcessConfigs (..)
    , PtAppConfig
    , PtHeaderConfig
    , PtHeadersConfig
    , PtPostProcessConfig
    , PtPostProcessConfigs
    , PtUpdateCopyrightConfig
    , UpdateCopyrightConfig (..)
    )
import Headroom.Data.Lens (suffixLenses)
import Headroom.FileType.Types (FileType (..))
import Headroom.Meta
    ( buildVersion
    , configBreakingChanges
    )
import RIO
import qualified RIO.ByteString as B

suffixLenses ''PostProcessConfig
suffixLenses ''PostProcessConfigs
suffixLenses ''UpdateCopyrightConfig

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Loads and parses application configuration from given /YAML/ file.
loadAppConfig :: (MonadIO m, MonadThrow m) => FilePath -> m PtAppConfig
loadAppConfig path = do
    content <- liftIO $ B.readFile path
    _ <- checkCompatibility configBreakingChanges buildVersion content
    parseAppConfig content

-- | Parses application configuration from given raw input in /YAML/ format.
parseAppConfig
    :: (MonadThrow m)
    => ByteString
    -- ^ raw input to parse
    -> m PtAppConfig
    -- ^ parsed application configuration
parseAppConfig = decodeConfiguration ProjectConfiguration

-- | Makes full 'CtAppConfig' from provided 'PtAppConfig' (if valid).
makeAppConfig
    :: (MonadThrow m)
    => PtAppConfig
    -- ^  source 'PtAppConfig'
    -> m CtAppConfig
    -- ^ full 'CtAppConfig'
makeAppConfig pt = do
    acRunMode <- lastOrError CkRunMode (acRunMode pt)
    acSourcePaths <- lastOrError CkSourcePaths (acSourcePaths pt)
    acExcludedPaths <- lastOrError CkExcludedPaths (acExcludedPaths pt)
    acExcludeIgnoredPaths <-
        lastOrError
            CkExcludeIgnoredPaths
            (acExcludeIgnoredPaths pt)
    acBuiltInTemplates <- lastOrError CkBuiltInTemplates (acBuiltInTemplates pt)
    acTemplateRefs <- pure $ acTemplateRefs pt
    acLicenseHeaders <- makeHeadersConfig (acLicenseHeaders pt)
    acPostProcessConfigs <- makePostProcessConfigs (acPostProcessConfigs pt)
    acVariables <- pure $ acVariables pt
    pure AppConfig{..}

-- | Makes full 'CtHeadersConfig' from provided 'PtHeadersConfig' (if valid).
makeHeadersConfig
    :: (MonadThrow m)
    => PtHeadersConfig
    -- ^ source 'PtHeadersConfig'
    -> m CtHeadersConfig
    -- ^ full 'CtHeadersConfig'
makeHeadersConfig pt = do
    hscC <- makeHeaderConfig C (hscC pt)
    hscCpp <- makeHeaderConfig CPP (hscCpp pt)
    hscCss <- makeHeaderConfig CSS (hscCss pt)
    hscDart <- makeHeaderConfig Dart (hscDart pt)
    hscGo <- makeHeaderConfig Go (hscGo pt)
    hscHaskell <- makeHeaderConfig Haskell (hscHaskell pt)
    hscHtml <- makeHeaderConfig HTML (hscHtml pt)
    hscJava <- makeHeaderConfig Java (hscJava pt)
    hscJs <- makeHeaderConfig JS (hscJs pt)
    hscKotlin <- makeHeaderConfig Kotlin (hscKotlin pt)
    hscPhp <- makeHeaderConfig PHP (hscPhp pt)
    hscPureScript <- makeHeaderConfig PureScript (hscPureScript pt)
    hscPython <- makeHeaderConfig Python (hscPython pt)
    hscRust <- makeHeaderConfig Rust (hscRust pt)
    hscScala <- makeHeaderConfig Scala (hscScala pt)
    hscShell <- makeHeaderConfig Shell (hscShell pt)
    hscXml <- makeHeaderConfig XML (hscXml pt)
    pure HeadersConfig{..}

-- | Makes full 'CtHeaderConfig' from provided 'PtHeaderConfig' (if valid).
makeHeaderConfig
    :: (MonadThrow m)
    => FileType
    -- ^ determines file type of configuration
    -> PtHeaderConfig
    -- ^ source 'PtHeaderConfig'
    -> m CtHeaderConfig
    -- ^ full 'CtHeaderConfig'
makeHeaderConfig fileType pt = do
    hcFileExtensions <-
        lastOrError
            (CkFileExtensions fileType)
            (hcFileExtensions pt)
    hcMarginTopCode <- lastOrError (CkMarginTopCode fileType) (hcMarginTopCode pt)
    hcMarginTopFile <- lastOrError (CkMarginTopFile fileType) (hcMarginTopFile pt)
    hcMarginBottomCode <-
        lastOrError
            (CkMarginBottomCode fileType)
            (hcMarginBottomCode pt)
    hcMarginBottomFile <-
        lastOrError
            (CkMarginBottomFile fileType)
            (hcMarginBottomFile pt)
    hcPutAfter <- lastOrError (CkPutAfter fileType) (hcPutAfter pt)
    hcPutBefore <- lastOrError (CkPutBefore fileType) (hcPutBefore pt)
    hcHeaderSyntax <- lastOrError (CkHeaderSyntax fileType) (hcHeaderSyntax pt)
    pure HeaderConfig{..}

------------------------------  PRIVATE FUNCTIONS  -----------------------------

makePostProcessConfigs
    :: (MonadThrow m)
    => PtPostProcessConfigs
    -> m CtPostProcessConfigs
makePostProcessConfigs pt = do
    ppcsUpdateCopyright <-
        makePostProcessConfig
            (pt ^. ppcsUpdateCopyrightL)
            makeUpdateCopyrightConfig
    pure PostProcessConfigs{..}

makePostProcessConfig
    :: (MonadThrow m)
    => PtPostProcessConfig c
    -> (c 'Partial -> m (c 'Complete))
    -> m (CtPostProcessConfig c)
makePostProcessConfig pt fn = do
    ppcEnabled <- lastOrError CkEnabled (pt ^. ppcEnabledL)
    ppcConfig <- fn $ pt ^. ppcConfigL
    pure PostProcessConfig{..}

makeUpdateCopyrightConfig
    :: (MonadThrow m)
    => PtUpdateCopyrightConfig
    -> m CtUpdateCopyrightConfig
makeUpdateCopyrightConfig pt = do
    let uccSelectedAuthors = lastOrNothing $ pt ^. uccSelectedAuthorsL
    pure UpdateCopyrightConfig{..}

lastOrError :: (MonadThrow m) => ConfigurationKey -> Last a -> m a
lastOrError key (Last a) = maybe (throwM $ MissingConfiguration key) pure a

lastOrNothing :: Last (Maybe a) -> Maybe a
lastOrNothing (Last a) = fromMaybe Nothing a
