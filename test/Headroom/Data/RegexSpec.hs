{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Data.RegexSpec
    ( spec
    )
where

import qualified Data.Aeson as A
import Headroom.Data.Regex
import RIO
import qualified RIO.Text as T
import Test.Hspec

spec :: Spec
spec = do
    describe "FromJSON instance for Regex" $ do
        it "deserializes a valid regular expression" $ do
            A.eitherDecode "\"foo|bar\"" `shouldBe` Right [re|foo|bar|]

        it "returns the compiler reason for an invalid regular expression" $ do
            (A.eitherDecode "\"[\"" :: Either String Regex)
                `shouldSatisfy` errorContaining "missing terminating ]"

        it "rejects a non-string regular expression through Parser" $ do
            (A.eitherDecode "[]" :: Either String Regex)
                `shouldSatisfy` errorContaining "expected String"

    describe "match" $ do
        it "matches regular expression against given sample" $ do
            let regex = [re|foo|bar|]
            match regex "xxx" `shouldSatisfy` isNothing
            match regex "foz" `shouldSatisfy` isNothing
            match regex "foosdas" `shouldSatisfy` isJust
            match regex "barfoo" `shouldSatisfy` isJust

    describe "isMatch" $ do
        it "checks if regular expression matches against given sample" $ do
            let regex = [re|foo|bar|]
            isMatch regex "foz" `shouldBe` False
            isMatch regex "xxx" `shouldBe` False
            isMatch regex "foosdas" `shouldBe` True
            isMatch regex "barfoo" `shouldBe` True

errorContaining :: Text -> Either String a -> Bool
errorContaining expected =
    either (T.isInfixOf expected . T.pack) (const False)
