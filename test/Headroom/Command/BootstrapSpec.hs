{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Command.BootstrapSpec
    ( spec
    )
where

import Headroom.Command.Bootstrap
    ( BootstrapEnv (..)
    , bootstrap
    )
import Headroom.Config.Global
    ( GlobalConfig (..)
    , UpdaterConfig (..)
    )
import Headroom.Data.Has (Has (..))
import Headroom.Data.Lens (suffixLenses)
import Headroom.IO.FileSystem
    ( FileSystem (..)
    , mkFileSystem
    )
import Headroom.IO.KVStore
    ( KVStore (..)
    , KVStoreError (..)
    )
import Headroom.IO.Network (Network (..))
import RIO
import Test.Hspec

data TestEnv = TestEnv
    { envLogFunc :: LogFunc
    , envFileSystem :: FileSystem (RIO TestEnv)
    , envKVStore :: KVStore (RIO TestEnv)
    , envNetwork :: Network (RIO TestEnv)
    }

suffixLenses ''TestEnv

instance HasLogFunc TestEnv where
    logFuncL = envLogFuncL

instance Has (FileSystem (RIO TestEnv)) TestEnv where
    hasLens = envFileSystemL

instance Has (KVStore (RIO TestEnv)) TestEnv where
    hasLens = envKVStoreL

instance Has (Network (RIO TestEnv)) TestEnv where
    hasLens = envNetworkL

spec :: Spec
spec = do
    describe "bootstrap" $ do
        it "logs a store failure and continues bootstrapping" $ do
            withSystemTempDirectory "bootstrap" $ \dir -> do
                levelsRef <- newIORef []
                let env = testEnv dir levelsRef
                result <- runRIO env bootstrap
                beGlobalConfig result `shouldBe` GlobalConfig (UpdaterConfig True 7)
                levels <- readIORef levelsRef
                levels `shouldSatisfy` elem LevelWarn

testEnv :: FilePath -> IORef [LogLevel] -> TestEnv
testEnv userDirectory levelsRef =
    TestEnv
        { envLogFunc = mkLogFunc $ \_ _ level _ -> modifyIORef' levelsRef (level :)
        , envFileSystem = mkFileSystem{fsGetUserDirectory = pure userDirectory}
        , envKVStore =
            KVStore
                { kvGetValue = const storeFailure
                , kvPutValue = \_ _ -> storeFailure
                }
        , envNetwork = Network{nDownloadContent = const unexpectedNetworkAccess}
        }
  where
    storeFailure = throwM $ CannotAccessStore "test-store" "database is locked"
    unexpectedNetworkAccess = throwString "unexpected network access"
