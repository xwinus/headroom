{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.UI.ProgressSpec
    ( spec
    )
where

import Headroom.UI.Progress
import RIO
import Test.Hspec

spec :: Spec
spec = do
    describe "zipWithProgress" $ do
        it "zips progress for given collection" $ do
            let col = ["a", "b"] :: [Text]
                expected = [(Progress 1 2, "a"), (Progress 2 2, "b")]
            zipWithProgress col `shouldBe` expected

    describe "Display instance" $ do
        it "displays correct output for Progress data type" $ do
            textDisplay (Progress 1 1) `shouldBe` "[1 of 1]"
            textDisplay (Progress 10 250) `shouldBe` "[ 10 of 250]"

        it "displays progress when the final total is not known" $ do
            textDisplay (CurrentProgress 42) `shouldBe` "[42]"
