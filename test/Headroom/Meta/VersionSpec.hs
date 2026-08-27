{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Meta.VersionSpec
    ( spec
    )
where

import qualified Data.Aeson as A
import Headroom.Meta.Version
import RIO
import qualified RIO.Text as T
import Test.Hspec

spec :: Spec
spec = do
    describe "FromJSON instance for Version" $ do
        it "deserializes a valid PVP version" $ do
            A.eitherDecode "\"0.1.2.3\"" `shouldBe` Right [pvp|0.1.2.3|]

        it "rejects an invalid PVP version through Parser" $ do
            (A.eitherDecode "\"0.1\"" :: Either String Version)
                `shouldSatisfy` errorContaining "not valid PVP version"

        it "rejects a non-string PVP version through Parser" $ do
            (A.eitherDecode "123" :: Either String Version)
                `shouldSatisfy` errorContaining "expected String"

    describe "Ord instance" $ do
        it "correctly compare two values" $ do
            compare [pvp|1.2.3.4|] [pvp|1.2.3.4|] `shouldBe` EQ
            compare [pvp|0.1.0.0|] [pvp|0.1.0.1|] `shouldBe` LT
            compare [pvp|1.1.0.0|] [pvp|0.1.0.1|] `shouldBe` GT

    describe "parseVersion" $ do
        it "parses valid Version from Text" $ do
            parseVersion "0.1.2.3" `shouldBe` Just [pvp|0.1.2.3|]
            parseVersion "v0.1.2.3" `shouldBe` Just [pvp|0.1.2.3|]

        it "parses Nothing for invalid input" $ do
            parseVersion "0.4" `shouldBe` Nothing

    describe "printVersion" $ do
        it "pretty prints given version" $ do
            printVersion [pvp|1.2.3.4|] `shouldBe` "1.2.3.4"

    describe "printVersionP" $ do
        it "pretty prints given version (with 'v' prefix)" $ do
            printVersionP [pvp|1.2.3.4|] `shouldBe` "v1.2.3.4"

    describe "pvp" $ do
        it "produces correct Version using QuasiQuotes" $ do
            [pvp|0.1.2.3|] `shouldBe` Version 0 1 2 3

errorContaining :: Text -> Either String a -> Bool
errorContaining expected =
    either (T.isInfixOf expected . T.pack) (const False)
