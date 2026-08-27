{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.UpdaterSpec
    ( spec
    )
where

import Data.Aeson (Value)
import qualified Data.Aeson as A
import Data.String.Interpolate (i)
import Data.Time (UTCTime (..))
import Headroom.Config.Global (UpdaterConfig (..))
import Headroom.Data.Has (Has (..))
import Headroom.Data.Lens
    ( suffixLenses
    , suffixLensesFor
    )
import Headroom.IO.KVStore
    ( KVStore (..)
    , inMemoryKVStore
    , valueKey
    )
import Headroom.IO.Network
    ( Network (..)
    , NetworkError (..)
    )
import Headroom.Meta (buildVersion)
import Headroom.Meta.Version
    ( printVersionP
    , pvp
    )
import Headroom.Updater
import RIO
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import RIO.FilePath ((</>))
import RIO.Partial (fromJust)
import RIO.Time
    ( addDays
    , getCurrentTime
    )
import Test.Hspec

data TestEnv = TestEnv
    { envKVStore :: KVStore (RIO TestEnv)
    , envNetwork :: Network (RIO TestEnv)
    }

suffixLenses ''TestEnv
suffixLensesFor ["nDownloadContent"] ''Network

instance Has (KVStore (RIO TestEnv)) TestEnv where
    hasLens = envKVStoreL

instance Has (Network (RIO TestEnv)) TestEnv where
    hasLens = envNetworkL

spec :: Spec
spec = do
    let testFile = "test-data" </> "updater" </> "github-resp.json"
        storeKey = valueKey @UTCTime "updater/last-check-date"

    describe "checkUpdates" $ do
        it "returns Nothing if current version is the latest" $ do
            store@KVStore{..} <- runRIO env0 inMemoryKVStore
            let json = [i|{"name": "#{printVersionP buildVersion}"}|]
                env =
                    env0
                        & (envKVStoreL .~ kvStore')
                        & (envNetworkL . nDownloadContentL .~ nDownloadContent')
                nDownloadContent' = const . pure $ json
                updaterConfig' = UpdaterConfig True 2
                kvStore' = store
            actual <- runRIO env (checkUpdates updaterConfig')
            maybeLastCheckDate <- runRIO env $ kvGetValue storeKey
            actual `shouldBe` Nothing
            maybeLastCheckDate `shouldSatisfy` isJust

        it "returns newer version if current version is outdated" $ do
            store@KVStore{..} <- runRIO env0 inMemoryKVStore
            let json = [i|{"name": "v999.0.0.0"}|]
                env =
                    env0
                        & (envKVStoreL .~ kvStore')
                        & (envNetworkL . nDownloadContentL .~ nDownloadContent')
                nDownloadContent' = const . pure $ json
                updaterConfig' = UpdaterConfig True 2
                kvStore' = store
            actual <- runRIO env (checkUpdates updaterConfig')
            maybeLastCheckDate <- runRIO env $ kvGetValue storeKey
            actual `shouldBe` Just [pvp|999.0.0.0|]
            maybeLastCheckDate `shouldSatisfy` isJust

        it "doesn't check for updates if still within check interval" $ do
            store@KVStore{..} <- runRIO env0 inMemoryKVStore
            oneDayAgo <- addDays' (-1) <$> getCurrentTime
            let json = [i|{"name": "v999.0.0.0"}|]
                env =
                    env0
                        & (envKVStoreL .~ kvStore')
                        & (envNetworkL . nDownloadContentL .~ nDownloadContent')
                nDownloadContent' = const . pure $ json
                updaterConfig' = UpdaterConfig True 2
                kvStore' = store
            actual <- runRIO env $ do
                kvPutValue storeKey oneDayAgo
                checkUpdates updaterConfig'
            actual `shouldBe` Nothing

        it "returns Nothing if checking for updates is disabled" $ do
            runRIO env0 (checkUpdates $ UpdaterConfig False 2) `shouldReturn` Nothing

    describe "fetchLatestVersion" $ do
        it "gets latest version info" $ do
            raw <- B.readFile testFile
            let env = env0 & (envNetworkL . nDownloadContentL .~ nDownloadContent')
                nDownloadContent' = const . pure $ raw
            actual <- runRIO env fetchLatestVersion
            actual `shouldBe` [pvp|0.4.2.0|]

        it "returns error if version cannot be fetched" $ do
            let env = env0 & (envNetworkL . nDownloadContentL .~ nDownloadContent')
                nDownloadContent' = \uri -> throwM $ ConnectionFailure uri "error"
            runRIO env fetchLatestVersion `shouldThrow` \case
                (CannotDetectVersion _) -> True

    describe "parseLatestVersion" $ do
        it "parses latest version from raw JSON input" $ do
            raw <- BL.readFile testFile
            actual <- parseLatestVersion (fromJust . A.decode @Value $ raw)
            actual `shouldBe` [pvp|0.4.2.0|]

env0 :: TestEnv
env0 =
    TestEnv
        { envKVStore = KVStore{kvGetValue = undefined, kvPutValue = undefined}
        , envNetwork = Network{nDownloadContent = undefined}
        }

addDays' :: Integer -> UTCTime -> UTCTime
addDays' noOfDays (UTCTime day timeOfDay) =
    UTCTime (addDays noOfDays day) timeOfDay
