{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.IO.KVStoreSpec
    ( spec
    )
where

import Control.Monad (replicateM)
import Headroom.IO.KVStore
import RIO
import RIO.FilePath ((</>))
import qualified RIO.Text as T
import RIO.Time
import Test.Hspec

spec :: Spec
spec = do
    describe "SQLite store" $ do
        it "reads and writes values from/to store" $ do
            withSystemTempDirectory "sqlite-kvstore" $ \dir -> do
                let path = StorePath . T.pack $ dir </> "test-db.sqlite"
                    fstKey = valueKey @Text "fst-key"
                    sndKey = valueKey @Text "snd-key"
                KVStore{..} <- sqliteKVStore path
                maybeFst <- kvGetValue fstKey
                _ <- kvPutValue sndKey "foo"
                _ <- kvPutValue sndKey "bar"
                maybeSnd <- kvGetValue sndKey
                maybeFst `shouldBe` Nothing
                maybeSnd `shouldBe` Just "bar"

        it "survives concurrent access from separate connections" $ do
            withSystemTempDirectory "sqlite-kvstore" $ \dir -> do
                let path = StorePath . T.pack $ dir </> "test-db.sqlite"
                    key = valueKey @Text "shared-key"
                -- each store has its own connection pool, mimicking the way
                -- multiple Headroom processes share single store file
                stores <- replicateM 4 $ sqliteKVStore path
                results <- forConcurrently stores $ \KVStore{..} -> do
                    replicateM_ 4 $ kvPutValue key "foo"
                    kvGetValue key
                results `shouldBe` replicate 4 (Just "foo")

    describe "In-memory store" $ do
        it "reads and writes values from/to store" $ do
            let fstKey = valueKey @Text "fst-key"
                sndKey = valueKey @Text "snd-key"
            KVStore{..} <- inMemoryKVStore
            maybeFst <- kvGetValue fstKey
            _ <- kvPutValue sndKey "foo"
            _ <- kvPutValue sndKey "bar"
            maybeSnd <- kvGetValue sndKey
            maybeFst `shouldBe` Nothing
            maybeSnd `shouldBe` Just "bar"

    describe "ValueCodec type class" $ do
        it "has working instance for Text" $ do
            let sample = "The Cake is a Lie"
            decodeValue @Text (encodeValue sample) `shouldBe` Just sample

        it "has working instance for UTCTime" $ do
            sample <- getCurrentTime
            decodeValue @UTCTime (encodeValue sample) `shouldBe` Just sample
