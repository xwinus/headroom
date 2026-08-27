{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.ConfigSpec
    ( spec
    )
where

import qualified Data.Aeson as A
import Headroom.Config
import Headroom.Config.Parse
    ( ConfigurationParseError (..)
    , ConfigurationScope (..)
    )
import Headroom.Config.Types
    ( PtAppConfig
    , RunMode (..)
    )
import Headroom.Embedded (defaultConfig)
import RIO
import qualified RIO.Text as T
import Test.Hspec

spec :: Spec
spec = do
    describe "parseAppConfig" $ do
        it "should parse default bundled configuration" $ do
            parseAppConfig defaultConfig `shouldSatisfy` isJust

        it "returns a structured error for an invalid configuration value" $ do
            (parseAppConfig "run-mode: typo" :: IO PtAppConfig)
                `shouldThrow` configurationErrorContaining "Unknown run mode: typo"

        it "returns a structured error for malformed YAML" $ do
            (parseAppConfig "run-mode: [" :: IO PtAppConfig)
                `shouldThrow` configurationErrorContaining "YAML parse exception"

    describe "FromJSON instance for RunMode" $ do
        it "deserializes a known run mode" $ do
            A.eitherDecode "\"CHECK\"" `shouldBe` Right Check

        it "rejects an unknown run mode through Parser" $ do
            (A.eitherDecode "\"typo\"" :: Either String RunMode)
                `shouldSatisfy` errorContaining "Unknown run mode: typo"

        it "rejects a non-string run mode through Parser" $ do
            (A.eitherDecode "42" :: Either String RunMode)
                `shouldSatisfy` errorContaining "expected String"

configurationErrorContaining :: Text -> ConfigurationParseError -> Bool
configurationErrorContaining expected (ConfigurationParseError scope parseError) =
    scope
        == ProjectConfiguration
        && expected
        `T.isInfixOf` T.pack (displayException parseError)

errorContaining :: Text -> Either String a -> Bool
errorContaining expected =
    either (T.isInfixOf expected . T.pack) (const False)
