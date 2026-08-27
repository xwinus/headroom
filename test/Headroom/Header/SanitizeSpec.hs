{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Header.SanitizeSpec
    ( spec
    )
where

import Headroom.Config.Types (HeaderSyntax (..))
import Headroom.Data.Regex (re)
import Headroom.Data.Text (fromLines)
import Headroom.Header.Sanitize
import RIO
import Test.Hspec

spec :: Spec
spec = do
    describe "findPrefix" $ do
        it "finds and fills line prefix to LineComment header syntax" $ do
            let s = [re|^--|]
                sample = fromLines ["-- first", "", "-- second", "-- third"]
                syntax = LineComment s Nothing
                expected = LineComment s (Just "--")
            findPrefix syntax sample `shouldBe` expected

        it "detects only comment syntax in a single-line variable template" $ do
            let s = [re|^#|]
                template = "# Copyright {{year}}"
                rendered = "# Copyright 2026"
                syntax = findPrefix (LineComment s Nothing) template
            syntax `shouldBe` LineComment s (Just "#")
            sanitizeSyntax syntax rendered `shouldBe` rendered

        it "detects only comment syntax in a single-line static template" $ do
            let s = [re|^#|]
                template = "# Copyright 2026"
                syntax = findPrefix (LineComment s Nothing) template
            syntax `shouldBe` LineComment s (Just "#")
            sanitizeSyntax syntax template `shouldBe` template

        it "preserves whitespace matched by the line comment syntax" $ do
            let s = [re|^#\h+|]
                sample = "# Copyright"
                syntax = LineComment s Nothing
                expected = LineComment s (Just "# ")
            findPrefix syntax sample `shouldBe` expected

        it "leaves line prefix unknown when comment syntax does not match" $ do
            let s = [re|^#|]
                sample = "Copyright {{year}}"
                syntax = LineComment s Nothing
            findPrefix syntax sample `shouldBe` syntax

        it "finds and fills line prefix to BlockComment header syntax" $ do
            let s = [re|^{-\||]
                e = [re|(?<!#)-}$|]
                sample = fromLines ["{-|", " - foo", " - bar", " -}"]
                syntax = BlockComment s e Nothing
                expected = BlockComment s e (Just " -")
            findPrefix syntax sample `shouldBe` expected

    describe "sanitizeSyntax" $ do
        it "sanitizes syntax for line comment with prefix" $ do
            let syntax = LineComment [re|^--|] (Just "--")
                sample = fromLines ["-- 1", "2", "-- 3", " 4"]
                expected = fromLines ["-- 1", "-- 2", "-- 3", "-- 4"]
            sanitizeSyntax syntax sample `shouldBe` expected

        it "sanitizes syntax for block comment with prefix" $ do
            let syntax = BlockComment [re|^\/\*|] [re|\*\/$|] (Just " *")
                sample = fromLines ["/*", " * 1", "2", " 3", " */"]
                expected = fromLines ["/*", " * 1", " * 2", " * 3", " */"]
            sanitizeSyntax syntax sample `shouldBe` expected

        it "does nothing for already valid header" $ do
            let syntax = LineComment [re|^--|] (Just "--")
                sample = fromLines ["-- first", "-- second", "-- third"]
            sanitizeSyntax syntax sample `shouldBe` sample

        it "does nothing when prefix is unknown" $ do
            let syntax = LineComment [re|^--|] Nothing
                sample = fromLines ["-- first", "second", "-- third"]
            sanitizeSyntax syntax sample `shouldBe` sample

    describe "stripCommentSyntax" $ do
        it "strips comment syntax from single line block comment (no prefix)" $ do
            let syntax = BlockComment [re|^{-\||] [re|(?<!#)-}$|] Nothing
                sample = "{-| outer {- and inner -} comment -}"
                expected = " outer {- and inner -} comment "
            stripCommentSyntax syntax sample `shouldBe` expected

        it "strips comment syntax from single line line comment (no prefix)" $ do
            let syntax = LineComment [re|^--|] Nothing
                sample = "-- single line comment"
                expected = " single line comment"
            stripCommentSyntax syntax sample `shouldBe` expected

        it "strips comment syntax from multi line block comment (no prefix)" $ do
            let syntax = BlockComment [re|^{-\||] [re|(?<!#)-}$|] Nothing
                sample =
                    fromLines
                        [ "{-|"
                        , "Some block comment"
                        , "another line"
                        , "@"
                        , "  {- code example comment -}"
                        , "@"
                        , "-}"
                        ]
                expected =
                    fromLines
                        [ ""
                        , "Some block comment"
                        , "another line"
                        , "@"
                        , "  {- code example comment -}"
                        , "@"
                        , ""
                        ]
            stripCommentSyntax syntax sample `shouldBe` expected

        it "strips comment syntax from multi line block comment (with prefix)" $ do
            let syntax = BlockComment [re|^\/\*|] [re|\*\/$|] (Just " * ")
                sample =
                    fromLines
                        [ "/*"
                        , " * Some block comment"
                        , " * another line"
                        , " * @"
                        , " *  /* code example comment */"
                        , " */"
                        ]
                expected =
                    fromLines
                        [ ""
                        , "Some block comment"
                        , "another line"
                        , "@"
                        , " /* code example comment */"
                        , " "
                        ]
            stripCommentSyntax syntax sample `shouldBe` expected

        it "strips comment syntax from multi line line comment" $ do
            let syntax = LineComment [re|^--|] (Just "-- ")
                sample =
                    fromLines
                        [ "-- Some block comment"
                        , "-- another line"
                        , "-- @"
                        , "--  /* code example comment */"
                        ]
                expected =
                    fromLines
                        [ "Some block comment"
                        , "another line"
                        , "@"
                        , " /* code example comment */"
                        ]
            stripCommentSyntax syntax sample `shouldBe` expected
