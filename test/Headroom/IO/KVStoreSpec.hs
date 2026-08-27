{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.IO.KVStoreSpec
    ( spec
    )
where

import qualified Database.Sqlite as SQLite
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
                let KVStore{..} = sqliteKVStore path
                maybeFst <- kvGetValue fstKey
                _ <- kvPutValue sndKey "foo"
                _ <- kvPutValue sndKey "bar"
                maybeSnd <- kvGetValue sndKey
                maybeFst `shouldBe` Nothing
                maybeSnd `shouldBe` Just "bar"

        it "retries a write until a lock held by another connection is released" $ do
            withSystemTempDirectory "sqlite-kvstore" $ \dir -> do
                let rawPath = T.pack $ dir </> "test-db.sqlite"
                    path = StorePath rawPath
                    key = valueKey @Text "shared-key"
                    KVStore{..} = sqliteKVStore path
                kvPutValue key "before-lock"
                withWriteLock rawPath $ \connection -> do
                    writer <- async $ kvPutValue key "after-lock"
                    threadDelay 250000
                    poll writer >>= (`shouldSatisfy` isNothing)
                    execute connection "COMMIT;"
                    wait writer
                kvGetValue key `shouldReturn` Just "after-lock"

        it "returns a typed error when the lock outlives the retry budget" $ do
            withSystemTempDirectory "sqlite-kvstore" $ \dir -> do
                let rawPath = T.pack $ dir </> "test-db.sqlite"
                    path = StorePath rawPath
                    key = valueKey @Text "shared-key"
                    KVStore{..} = sqliteKVStore path
                kvPutValue key "before-lock"
                withWriteLock rawPath $ \_ ->
                    kvPutValue key "blocked" `shouldThrow` \case
                        CannotAccessStore actualPath reason ->
                            actualPath == rawPath && "timed out" `T.isInfixOf` reason

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

withWriteLock :: Text -> (SQLite.Connection -> IO a) -> IO a
withWriteLock path action = bracket (SQLite.open path) SQLite.close $ \connection -> do
    execute connection "BEGIN IMMEDIATE;"
    action connection

execute :: SQLite.Connection -> Text -> IO ()
execute connection query =
    bracket (SQLite.prepare connection query) SQLite.finalize $ \statement ->
        void $ SQLite.step statement
