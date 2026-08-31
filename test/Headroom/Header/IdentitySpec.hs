{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Header.IdentitySpec (spec) where

import Headroom.Config.Types
    ( CtHeaderConfig
    , HeaderConfig (..)
    , HeaderSyntax (..)
    )
import Headroom.Data.Regex (re)
import Headroom.FileType.Types (FileType (..))
import Headroom.Header
    ( addHeader
    , dropHeader
    , findHeader
    , identifyHeader
    , replaceHeader
    )
import Headroom.Header.Marker
    ( endMarker
    , markHeader
    , startMarker
    )
import Headroom.Header.Types
    ( HeaderDetection (..)
    , HeaderInfo (..)
    , HeaderOrigin (..)
    )
import Headroom.SourceCode
    ( LineType (..)
    , SourceCode (..)
    )
import RIO
import qualified RIO.Text as T
import Test.Hspec

spec :: Spec
spec = do
    describe "managed header identity" $ do
        it "drops a marked line header without dropping adjacent Go docs" $ do
            let expected = "// Copyright 2026 Example\n// SPDX-License-Identifier: MIT"
                marked = commentSource . T.lines $ markHeader lineSyntax expected
                docs = SourceCode [(Comment, "// Package api provides the public API.")]
                source = marked <> docs <> SourceCode [(Code, "package api")]
                info = identify lineConfig Go expected source
            hiHeaderDetection info
                `shouldBe` ManagedHeader HeadroomMarker (0, 3)
            dropHeader info source
                `shouldBe` docs <> SourceCode [(Code, "package api")]

        it "replaces a marked block header without dropping adjacent JSDoc" $ do
            let expected = "/*\n * Copyright 2026 Example\n */"
                replacement = "/*\n * Copyright 2027 Example\n */"
                marked = commentSource . T.lines $ markHeader blockSyntax expected
                docs = commentSource ["/**", " * Public API.", " */"]
                source = marked <> docs <> SourceCode [(Code, "export const api = {};")]
                info = identify blockConfig JS expected source
                replacement' = markHeader blockSyntax replacement
            replaceHeader info replacement' source
                `shouldBe` commentSource (T.lines replacement')
                    <> docs
                    <> SourceCode [(Code, "export const api = {};")]

        it "recognizes an exact unmarked legacy template" $ do
            let expected = "// Copyright 2026 Example\n// SPDX-License-Identifier: MIT"
                source = commentSource $ T.lines expected
                info = identify lineConfig Go expected source
            hiHeaderDetection info
                `shouldBe` ManagedHeader LegacyTemplate (0, 1)

        it "adds a header when there is no comment candidate" $ do
            let source = SourceCode [(Code, "package api")]
                info = identify lineConfig Go "// License" source
            hiHeaderDetection info `shouldBe` NoHeader
            addHeader info "// License" source
                `shouldBe` SourceCode [(Comment, "// License"), (Code, "package api")]

    describe "foreign comment safety" $ do
        it "preserves an ordinary leading Go documentation comment" $ do
            let source =
                    SourceCode
                        [ (Comment, "// Package api provides the public API.")
                        , (Code, "package api")
                        ]
                info = identify lineConfig Go "// Copyright 2026 Example" source
            hiHeaderDetection info `shouldBe` ForeignComment (0, 0)
            dropHeader info source `shouldBe` source
            replaceHeader info "// New license" source `shouldBe` source

        it "preserves an ordinary leading JavaScript block comment" $ do
            let source =
                    SourceCode
                        [ (Comment, "/**")
                        , (Comment, " * Initializes the application.")
                        , (Comment, " */")
                        , (Code, "bootstrap();")
                        ]
                info = identify blockConfig JS "/* license */" source
            hiHeaderDetection info `shouldBe` ForeignComment (0, 2)
            replaceHeader info "/* replacement */" source `shouldBe` source

        it "detects but preserves a PHP DocBlock after the opening tag" $ do
            let source =
                    SourceCode
                        [ (Code, "<?php")
                        , (Comment, "/**")
                        , (Comment, " * Application entry point.")
                        , (Comment, " */")
                        , (Code, "run();")
                        ]
                info = identify phpConfig PHP "/** license */" source
            findHeader phpConfig source `shouldBe` Just (1, 3)
            hiHeaderDetection info `shouldBe` ForeignComment (1, 3)
            dropHeader info source `shouldBe` source

        it "rejects an incomplete marker pair" $ do
            let source =
                    commentSource
                        [ "// " <> startMarker
                        , "// Copyright 2026 Example"
                        ]
                info = identify lineConfig Go "// Copyright 2026 Example" source
            hiHeaderDetection info `shouldBe` ForeignComment (0, 1)
            dropHeader info source `shouldBe` source

        it "rejects a marker pair outside the exact insertion boundary" $ do
            let source =
                    commentSource
                        [ "// Package api provides the public API."
                        , "// " <> startMarker
                        , "// Copyright 2026 Example"
                        , "// " <> endMarker
                        ]
                info = identify lineConfig Go "// Copyright 2026 Example" source
            hiHeaderDetection info `shouldBe` ForeignComment (0, 3)
            replaceHeader info "// replacement" source `shouldBe` source

        it "does not treat marker text embedded in prose as ownership" $ do
            let source =
                    commentSource
                        [ "// " <> startMarker <> " is documented here"
                        , "// Copyright 2026 Example"
                        , "// " <> endMarker
                        ]
                info = identify lineConfig Go "// Copyright 2026 Example" source
            hiHeaderDetection info `shouldBe` ForeignComment (0, 2)
            dropHeader info source `shouldBe` source

lineConfig :: CtHeaderConfig
lineConfig = HeaderConfig ["go"] 0 0 0 0 [] [] lineSyntax

lineSyntax :: HeaderSyntax
lineSyntax = LineComment [re|^//|] (Just "//")

blockConfig :: CtHeaderConfig
blockConfig = HeaderConfig ["js"] 0 0 0 0 [] [] blockSyntax

blockSyntax :: HeaderSyntax
blockSyntax = BlockComment [re|^/\*|] [re|\*/$|] (Just " *")

phpConfig :: CtHeaderConfig
phpConfig =
    HeaderConfig
        ["php"]
        0
        0
        0
        0
        [[re|^<\?php|]]
        []
        (BlockComment [re|^/\*\*|] [re|\*/$|] (Just " *"))

identify :: CtHeaderConfig -> FileType -> Text -> SourceCode -> HeaderInfo
identify config fileType expected source =
    identifyHeader expected source candidate
  where
    detection = maybe NoHeader ForeignComment $ findHeader config source
    candidate = HeaderInfo fileType config detection mempty

commentSource :: [Text] -> SourceCode
commentSource = SourceCode . fmap (Comment,)
