{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Command.Bootstrap
-- Description : Logic for bootstrapping Headroom
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Logic for running shared code and bootstrapping all /Headroom/ command /RIO/
-- applications.
module Headroom.Command.Bootstrap
    ( BootstrapEnv (..)
    , bootstrap
    , runRIO'
    , globalKVStore
    )
where

import Data.String.Interpolate (iii)
import Headroom.Config.Global
    ( GlobalConfig (..)
    , globalConfigPath
    , initGlobalConfigIfNeeded
    , loadGlobalConfig
    )
import Headroom.Data.Has
    ( Has (..)
    , HasRIO
    )
import Headroom.IO.FileSystem (FileSystem (..))
import Headroom.IO.KVStore
    ( KVStore
    , KVStoreError (..)
    , StorePath (..)
    , sqliteKVStore
    )
import Headroom.IO.Network (Network)
import Headroom.Meta
    ( cacheFileName
    , globalConfigDirName
    , productInfo
    , webRepo
    )
import Headroom.Meta.Version
    ( Version
    , printVersionP
    )
import Headroom.UI.Message (messageInfo)
import Headroom.Updater
    ( UpdaterError (..)
    , checkUpdates
    )
import RIO
import RIO.FilePath ((</>))
import qualified RIO.Text as T

-- | Bootstrap environment, containing pieces shared between all commands.
data BootstrapEnv = BootstrapEnv
    { beGlobalConfig :: GlobalConfig
    -- ^ loaded global configuration
    }

-- | Runs /RIO/ application using provided environment data and flag
-- whether to run in debug mode.
runRIO'
    :: (LogFunc -> IO env)
    -- ^ function returning environment data
    -> Bool
    -- ^ whether to run in debug mode
    -> RIO env a
    -- ^ /RIO/ application to execute
    -> IO a
    -- ^ execution result
runRIO' envFn isDebug logic = do
    defLogOptions <- logOptionsHandle stderr isDebug
    withLogFunc (setLogUseLoc False defLogOptions) $ \logFunc -> do
        env <- liftIO $ envFn logFunc
        runRIO env logic

-- | Executes the initialization logic that should be performed before any other
-- code is executed. During this bootstrap, for example /global configuration/
-- is initialized and loaded, welcome message is printed to console and updates
-- are checked.
bootstrap
    :: ( HasRIO FileSystem env
       , HasRIO KVStore env
       , HasRIO Network env
       , HasLogFunc env
       )
    => RIO env BootstrapEnv
bootstrap = do
    welcomeMessage
    initGlobalConfigIfNeeded
    globalConfig@GlobalConfig{..} <- loadGlobalConfig
    -- checking for updates is a convenience, so no failure of it (including
    -- failure of the store holding the date of the last check) may abort the run
    checkUpdates gcUpdates `catch` onUpdaterError `catch` onStoreError >>= \case
        Nothing -> pure ()
        Just newVersion -> displayUpdate newVersion
    pure BootstrapEnv{beGlobalConfig = globalConfig}
  where
    onUpdaterError err = warn $ displayException (err :: UpdaterError)
    onStoreError err = warn $ displayException (err :: KVStoreError)
    warn reason = do
        logWarn . display . T.pack $ reason
        pure Nothing

-- | Shared /SQLite/-based 'KVStore'.
globalKVStore :: (HasRIO FileSystem env) => RIO env (KVStore (RIO env))
globalKVStore = do
    FileSystem{..} <- viewL
    userDir <- fsGetUserDirectory
    pure
        . sqliteKVStore
        . StorePath
        . T.pack
        $ userDir
        </> globalConfigDirName
        </> cacheFileName

------------------------------  PRIVATE FUNCTIONS  -----------------------------

welcomeMessage :: (HasLogFunc env) => RIO env ()
welcomeMessage = logInfo . display $ productInfo

displayUpdate
    :: (HasRIO FileSystem env, HasLogFunc env)
    => Version
    -> RIO env ()
displayUpdate version = do
    configPath <- globalConfigPath
    logInfo . display . messageInfo $ message configPath
  where
    message configPath =
        [iii|
      New version #{printVersionP version} is available for download, you can
      get it from #{webRepo}.\n\t
      Tired of seeing this message? You can change the behaviour in global
      config file here:\n\t
      #{configPath}
    |]
