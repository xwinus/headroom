{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Command.Run.Config
-- Description : Configuration and time context for the run command.
-- Copyright   : (c) 2019-2023 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Headroom.Command.Run.Config
    ( currentYear
    , finalConfiguration
    )
where

import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime
    ( getCurrentTimeZone
    , localDay
    , utcToLocalTime
    )
import Headroom.Command.Types (CommandRunOptions (..))
import Headroom.Config
    ( loadAppConfig
    , makeAppConfig
    , parseAppConfig
    )
import Headroom.Config.Types
    ( AppConfig (..)
    , CtAppConfig
    , PtAppConfig
    )
import Headroom.Data.Has
    ( Has
    , viewL
    )
import Headroom.Embedded (defaultConfig)
import Headroom.Meta (configFileName)
import Headroom.Types (CurrentYear (..))
import Headroom.Variables (parseVariables)
import RIO

-- | Builds the final run configuration from defaults, YAML, and CLI options.
finalConfiguration
    :: (HasLogFunc env, Has CommandRunOptions env)
    => RIO env CtAppConfig
finalConfiguration = do
    defaultConfig' <- Just <$> parseAppConfig defaultConfig
    cmdLineConfig <- Just <$> optionsToConfiguration
    yamlConfig <- loadConfigurationSafe configFileName
    let mergedConfig =
            mconcat . catMaybes $ [defaultConfig', yamlConfig, cmdLineConfig]
    config <- makeAppConfig mergedConfig
    logDebug $ "Default config: " <> displayShow defaultConfig'
    logDebug $ "YAML config: " <> displayShow yamlConfig
    logDebug $ "CmdLine config: " <> displayShow cmdLineConfig
    logDebug $ "Merged config: " <> displayShow mergedConfig
    logDebug $ "Final config: " <> displayShow config
    pure config

-- | Obtains the current year in the local time zone.
currentYear :: (MonadIO m) => m CurrentYear
currentYear = do
    now <- liftIO getCurrentTime
    timezone <- liftIO getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
        (year, _, _) = toGregorian $ localDay zoneNow
    pure $ CurrentYear year

loadConfigurationSafe
    :: (HasLogFunc env)
    => FilePath
    -> RIO env (Maybe PtAppConfig)
loadConfigurationSafe path = catch (Just <$> loadAppConfig path) onError
  where
    onError err = do
        logDebug $ displayShow (err :: IOException)
        logInfo
            $ mconcat
                [ "Configuration file '"
                , fromString path
                , "' not found. You can either specify all required parameter by "
                , "command line arguments, or generate one using "
                , "'headroom gen -c >"
                , configFileName
                , "'. See official documentation "
                , "for more details."
                ]
        pure Nothing

optionsToConfiguration :: (Has CommandRunOptions env) => RIO env PtAppConfig
optionsToConfiguration = do
    CommandRunOptions{..} <- viewL
    variables <- parseVariables croVariables
    pure
        AppConfig
            { acRunMode = maybe mempty pure croRunMode
            , acSourcePaths = ifNot null croSourcePaths
            , acExcludedPaths = ifNot null croExcludedPaths
            , acExcludeIgnoredPaths = ifNot not croExcludeIgnoredPaths
            , acBuiltInTemplates = pure croBuiltInTemplates
            , acTemplateRefs = croTemplateRefs
            , acVariables = variables
            , acLicenseHeaders = mempty
            , acPostProcessConfigs = mempty
            }
  where
    ifNot condition value = if condition value then mempty else pure value
