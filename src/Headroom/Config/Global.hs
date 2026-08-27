{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Config.Global
-- Description : Global configutation
-- Copyright   : (c) 2019-2023 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- /Global configuration/ is configuration shared between all /Headroom/ instances
-- and it's located in user's home directory.
module Headroom.Config.Global
    ( GlobalConfig (..)
    , UpdaterConfig (..)
    , initGlobalConfigIfNeeded
    , loadGlobalConfig
    , parseGlobalConfig
    , globalConfigPath
    )
where

import Data.Aeson
    ( FromJSON (..)
    , genericParseJSON
    )
import Headroom.Config.Parse
    ( ConfigurationScope (GlobalConfiguration)
    , decodeConfiguration
    )
import Headroom.Data.Has
    ( Has (..)
    , HasRIO
    )
import Headroom.Data.Serialization (aesonOptions)
import Headroom.Embedded (defaultGlobalConfig)
import Headroom.IO.FileSystem (FileSystem (..))
import Headroom.Meta
    ( globalConfigDirName
    , globalConfigFileName
    )
import RIO
import qualified RIO.ByteString as B
import RIO.FilePath ((</>))

---------------------------------  DATA TYPES  ---------------------------------

-- | Data type representing updater configuration.
data UpdaterConfig = UpdaterConfig
    { ucCheckForUpdates :: Bool
    -- ^ whether to check for updates
    , ucUpdateIntervalDays :: Integer
    -- ^ how ofter check for updates
    }
    deriving (Eq, Generic, Show)

instance FromJSON UpdaterConfig where
    parseJSON = genericParseJSON aesonOptions

-- | Data type representing global configuration options.
data GlobalConfig = GlobalConfig
    { gcUpdates :: UpdaterConfig
    -- ^ config for updater
    }
    deriving (Eq, Generic, Show)

instance FromJSON GlobalConfig where
    parseJSON = genericParseJSON aesonOptions

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Checks if global configuration /YAML/ file is already present and if not,
-- it creates one with default values.
initGlobalConfigIfNeeded :: (HasRIO FileSystem env) => RIO env ()
initGlobalConfigIfNeeded = do
    FileSystem{..} <- viewL
    userDir <- fsGetUserDirectory
    configPath <- globalConfigPath
    whenM (not <$> fsDoesFileExist configPath) $ do
        fsCreateDirectory $ userDir </> globalConfigDirName
        fsWriteFile configPath defaultGlobalConfig

-- | Loads global configuration from /YAML/ file.
loadGlobalConfig :: (HasRIO FileSystem env) => RIO env GlobalConfig
loadGlobalConfig = do
    configPath <- globalConfigPath
    content <- liftIO . B.readFile $ configPath
    parseGlobalConfig content

-- | Parses global configuration /YAML/ file.
parseGlobalConfig :: (MonadThrow m) => ByteString -> m GlobalConfig
parseGlobalConfig = decodeConfiguration GlobalConfiguration

-- | Path to global configuration /YAML/ file in user's directory.
globalConfigPath :: (HasRIO FileSystem env) => RIO env FilePath
globalConfigPath = do
    FileSystem{..} <- viewL
    userDir <- fsGetUserDirectory
    pure $ userDir </> globalConfigDirName </> globalConfigFileName
