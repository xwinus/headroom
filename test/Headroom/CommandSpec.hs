{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.CommandSpec
    ( spec
    )
where

import Headroom.Command (commandParser)
import Headroom.Command.Types (Command (..))
import Options.Applicative
    ( defaultPrefs
    , execParserPure
    , getParseResult
    )
import RIO
import Test.Hspec

spec :: Spec
spec = do
    describe "run command" $ do
        it "parses source paths given as options" $ do
            sourcePathsOf ["run", "-s", "foo", "--source-path", "bar"]
                `shouldBe` Just ["foo", "bar"]

        it "parses source paths given as positional arguments" $ do
            sourcePathsOf ["run", "foo", "bar"] `shouldBe` Just ["foo", "bar"]

        it "combines source paths given as options and positional arguments" $ do
            sourcePathsOf ["run", "-s", "foo", "bar", "baz"]
                `shouldBe` Just ["foo", "bar", "baz"]

        it "parses positional arguments interspersed with other options" $ do
            sourcePathsOf ["run", "foo", "--dry-run", "bar"]
                `shouldBe` Just ["foo", "bar"]

        it "parses no source paths at all" $ do
            sourcePathsOf ["run"] `shouldBe` Just []

sourcePathsOf :: [String] -> Maybe [FilePath]
sourcePathsOf args = do
    command' <- getParseResult $ execParserPure defaultPrefs commandParser args
    case command' of
        Run paths _ _ _ _ _ _ _ _ -> pure paths
        _ -> Nothing
