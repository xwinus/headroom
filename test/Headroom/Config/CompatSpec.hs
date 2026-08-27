{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Config.CompatSpec
    ( spec
    )
where

import Headroom.Config.Compat
import Headroom.Meta.Version
    ( Version (..)
    , pvp
    )
import RIO
import qualified RIO.Text as T
import Test.Hspec

spec :: Spec
spec = do
    describe "checkCompatibility" $ do
        it "passes when config version is compatible" $ do
            let yaml = "version: 0.4.0.0"
                curr = [pvp|0.4.0.0|]
                version = [pvp|0.4.0.0|]
                versions = [[pvp|0.1.0.0|], [pvp|0.2.1.0|], [pvp|0.4.0.0|]]
            checkCompatibility versions curr yaml `shouldBe` Just version

        it "fails when config version is not compatible" $ do
            let yaml = "version: 0.2.1.0"
                curr = [pvp|0.2.1.0|]
                versions = [[pvp|0.1.0.0|], [pvp|0.2.1.0|], [pvp|0.4.0.0|]]
            let err (UnsupportedVersion [Version 0 4 0 0] (Version 0 2 1 0)) = True
                err _ = False
            checkCompatibility versions curr yaml `shouldThrow` err

        it "fails when version is newer than Headroom version" $ do
            let yaml = "version: 0.2.1.0"
                curr = [pvp|0.2.0.0|]
                versions = [[pvp|0.1.0.0|]]
            let err (NewerVersionDetected (Version 0 2 1 0)) = True
                err _ = False
            checkCompatibility versions curr yaml `shouldThrow` err

        it "fails when config version cannot be determined" $ do
            let yaml = ""
                curr = [pvp|0.2.0.0|]
                versions = [[pvp|0.1.0.0|], [pvp|0.2.1.0|], [pvp|0.4.0.0|]]
            let err CannotParseVersion = True
                err _ = False
            checkCompatibility versions curr yaml `shouldThrow` err

        it "fails when source YAML has invalid syntax" $ do
            let yaml = "invalid: [:]"
                curr = [pvp|0.2.0.0|]
                versions = [[pvp|0.1.0.0|], [pvp|0.2.1.0|], [pvp|0.4.0.0|]]
            let err (CannotParseYaml _) = True
                err _ = False
            checkCompatibility versions curr yaml `shouldThrow` err

        it "preserves the reason for an invalid version value" $ do
            let yaml = "version: invalid"
                curr = [pvp|0.2.0.0|]
                versions = [[pvp|0.1.0.0|]]
            let err (CannotParseYaml parseError) =
                    "not valid PVP version"
                        `T.isInfixOf` T.pack (displayException parseError)
                err _ = False
            checkCompatibility versions curr yaml `shouldThrow` err
