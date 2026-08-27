{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.TypesSpec
    ( spec
    )
where

import qualified Data.Aeson as A
import Headroom.Types (LicenseType (..))
import Headroom.Variables (mkVariables)
import RIO
import qualified RIO.Text as T
import Test.Hspec

spec :: Spec
spec = do
    describe "FromJSON instance for LicenseType" $ do
        it "deserializes a known license type" $ do
            A.eitherDecode "\"mit\"" `shouldBe` Right MIT

        it "rejects an unknown license type through Parser" $ do
            (A.eitherDecode "\"unknown\"" :: Either String LicenseType)
                `shouldSatisfy` errorContaining "Unknown license type: unknown"

        it "rejects a non-string license type through Parser" $ do
            (A.eitherDecode "false" :: Either String LicenseType)
                `shouldSatisfy` errorContaining "expected String"

    describe "Semigroup Variables" $ do
        it "combines two instances of variables" $ do
            let sample1 = mkVariables [("fst", "v1"), ("snd", "v1")]
                sample2 = mkVariables [("snd", "v2"), ("trd", "v1")]
                expected = mkVariables [("trd", "v1"), ("snd", "v2"), ("fst", "v1")]
            (sample1 <> sample2) `shouldBe` expected

errorContaining :: Text -> Either String a -> Bool
errorContaining expected =
    either (T.isInfixOf expected . T.pack) (const False)
