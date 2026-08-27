{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Updater
-- Description : Update Manager for Headroom
-- Copyright   : (c) 2019-2023 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- /Update Manager (Updater)/ is responsible for fetching data about latest version
-- of /Headroom/ and informing user about available updates. In future versions, it
-- might be capable to update /Headroom/ binaries automatically.
module Headroom.Updater
    ( checkUpdates
    , fetchLatestVersion
    , parseLatestVersion

      -- * Error Data Types
    , UpdaterError (..)
    )
where

import Data.Aeson (Value (String))
import qualified Data.Aeson as A
import Data.String.Interpolate (iii)
import Data.Time (UTCTime (utctDay))
import Headroom.Config.Global (UpdaterConfig (..))
import Headroom.Data.Has
    ( Has (..)
    , HasRIO
    )
import Headroom.IO.KVStore
    ( KVStore (..)
    , valueKey
    )
import Headroom.IO.Network (Network (..))
import Headroom.Meta (buildVersion)
import Headroom.Meta.Version
    ( Version
    , parseVersion
    )
import Headroom.Types
    ( fromHeadroomError
    , toHeadroomError
    )
import Lens.Micro.Aeson (key)
import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import RIO.Time
    ( diffDays
    , getCurrentTime
    )
import qualified Text.URI as URI

-- | Check whether newer version is available (if enabled by configuration).
checkUpdates
    :: (HasRIO KVStore env, HasRIO Network env)
    => UpdaterConfig
    -> RIO env (Maybe Version)
checkUpdates UpdaterConfig{..} = do
    KVStore{..} <- viewL
    now <- getCurrentTime
    -- note that the store must not be touched at all when the updater is
    -- disabled, so that disabling it works as a workaround for a broken store
    shouldCheck <-
        if not ucCheckForUpdates
            then pure False
            else do
                maybeLastCheckDate <- kvGetValue lastCheckDateKey
                pure $ case utctDay <$> maybeLastCheckDate of
                    Just lastCheck ->
                        abs (diffDays lastCheck (utctDay now)) > ucUpdateIntervalDays
                    Nothing -> True
    when shouldCheck $ kvPutValue lastCheckDateKey now
    if shouldCheck then isNewer <$> fetchLatestVersion else pure Nothing
  where
    lastCheckDateKey = valueKey @UTCTime "updater/last-check-date"
    isNewer version
        | version > buildVersion = Just version
        | otherwise = Nothing

-- | Fetches and parses latest version from update server.
fetchLatestVersion :: (HasRIO Network env) => RIO env Version
fetchLatestVersion = do
    Network{..} <- viewL
    apiURI <- latestVersionApiURI
    resp <- catchAny (nDownloadContent apiURI) handleError
    case A.decode (BL.fromStrict resp) of
        Just json -> parseLatestVersion json
        _ -> throwM $ CannotDetectVersion "cannot fetch response"
  where
    handleError = throwM . CannotDetectVersion . T.pack . displayException
    latestVersionApiURI =
        URI.mkURI
            "https://api.github.com/repos/xwinus/headroom/releases/latest"

-- | Parses latest version number from /GitHub/ API response.
parseLatestVersion
    :: (MonadThrow m)
    => Value
    -- ^ raw JSON response from /GitHub/
    -> m Version
    -- ^ parsed version
parseLatestVersion json = case json ^? key "name" of
    Just (String rawValue) -> case parseVersion rawValue of
        Just version -> pure version
        _ -> throwM $ CannotDetectVersion "cannot parse version"
    _ -> throwM $ CannotDetectVersion "cannot parse response"

---------------------------------  ERROR TYPES  --------------------------------

-- | Error during processing updates.
data UpdaterError = CannotDetectVersion Text
    deriving (Eq, Show, Typeable)

instance Exception UpdaterError where
    displayException = displayException'
    toException = toHeadroomError
    fromException = fromHeadroomError

displayException' :: UpdaterError -> String
displayException' = \case
    CannotDetectVersion reason ->
        [iii|
    Cannot get latest Headroom version from update servers, reason: #{reason}.
  |]
