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
        it "drops an exact line header without dropping adjacent Go docs" $ do
            let expected = "// Copyright 2026 Example\n// SPDX-License-Identifier: MIT"
                docs = SourceCode [(Comment, "// Package api provides the public API.")]
                source = commentSource (T.lines expected) <> docs <> code "package api"
                info = identify lineConfig Go expected source
            hiHeaderDetection info
                `shouldBe` ManagedHeader ExactTemplate (0, 1)
            dropHeader info source `shouldBe` docs <> code "package api"

        it "replaces an exact block header without dropping adjacent JSDoc" $ do
            let expected = "/*\n * Copyright 2026 Example\n */"
                replacement = "/*\n * Copyright 2027 Example\n */"
                docs = commentSource ["/**", " * Public API.", " */"]
                source = commentSource (T.lines expected) <> docs <> code "bootstrap();"
                info = identify blockConfig JS expected source
            hiHeaderDetection info
                `shouldBe` ManagedHeader ExactTemplate (0, 2)
            replaceHeader info replacement source
                `shouldBe` commentSource (T.lines replacement) <> docs <> code "bootstrap();"

        it "recognizes an exact one-line block header" $ do
            let expected = "/* SPDX-License-Identifier: MIT */"
                source = commentSource [expected] <> code "bootstrap();"
                info = identify blockConfig JS expected source
            hiHeaderDetection info
                `shouldBe` ManagedHeader ExactTemplate (0, 0)

        it "recognizes a stale SPDX header by its content fingerprint" $ do
            let expected =
                    "// Copyright 2026 Example Inc.\n// SPDX-License-Identifier: MIT"
                stale =
                    "// Copyright 2024 Former Maintainer\n// SPDX-License-Identifier: MIT"
                source = commentSource $ T.lines stale
                info = identify lineConfig Go expected source
            hiHeaderDetection info
                `shouldBe` ManagedHeader TemplateFingerprint (0, 1)

        it "recognizes stale long license boilerplate without consuming docs" $ do
            let docs = SourceCode [(Comment, "// Package api provides the public API.")]
                source = commentSource (T.lines longStale) <> docs <> code "package api"
                info = identify lineConfig Go longExpected source
            hiHeaderDetection info
                `shouldBe` ManagedHeader TemplateFingerprint (0, 3)
            dropHeader info source `shouldBe` docs <> code "package api"

        it "adds the rendered header without Headroom metadata" $ do
            let source = code "package api"
                info = identify lineConfig Go "// License" source
                result = addHeader info "// License" source
            hiHeaderDetection info `shouldBe` NoHeader
            result `shouldBe` commentSource ["// License"] <> source
            sourceTexts result `shouldSatisfy` (not . any (T.isInfixOf "headroom:"))

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
                    commentSource ["/**", " * Initializes the application.", " */"]
                        <> code "bootstrap();"
                info = identify blockConfig JS "/* license */" source
            hiHeaderDetection info `shouldBe` ForeignComment (0, 2)
            replaceHeader info "/* replacement */" source `shouldBe` source

        it "detects but preserves a PHP DocBlock after the opening tag" $ do
            let source =
                    code "<?php"
                        <> commentSource ["/**", " * Application entry point.", " */"]
                        <> code "run();"
                info = identify phpConfig PHP "/** license */" source
            findHeader phpConfig source `shouldBe` Just (1, 3)
            hiHeaderDetection info `shouldBe` ForeignComment (1, 3)
            dropHeader info source `shouldBe` source

        it "preserves an ambiguous stale short copyright comment" $ do
            let expected = "// Copyright 2026 Example"
                source = commentSource ["// Copyright 2024 Someone Else"]
                info = identify lineConfig Go expected source
            hiHeaderDetection info `shouldBe` ForeignComment (0, 0)
            replaceHeader info expected source `shouldBe` source

        it "does not skip leading docs to claim a later header" $ do
            let expected = "// Copyright 2026 Example\n// SPDX-License-Identifier: MIT"
                source =
                    commentSource
                        [ "// Package api provides the public API."
                        , "// Copyright 2026 Example"
                        , "// SPDX-License-Identifier: MIT"
                        ]
                info = identify lineConfig Go expected source
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

longExpected :: Text
longExpected =
    T.unlines
        [ "// Copyright 2026 Example Inc."
        , "// Permission is hereby granted free of charge to any person obtaining a copy"
        , "// of this software and associated documentation files to deal in the software"
        , "// without restriction including rights to use copy modify merge publish distribute"
        ]

longStale :: Text
longStale =
    T.unlines
        [ "// Copyright 2024 Former Maintainer"
        , "// Permission is hereby granted free of charge to any person obtaining a copy"
        , "// of this software and associated documentation files to deal in the software"
        , "// without restriction including rights to use copy modify merge publish distribute"
        ]

identify :: CtHeaderConfig -> FileType -> Text -> SourceCode -> HeaderInfo
identify config fileType expected source =
    identifyHeader expected source candidate
  where
    detection = maybe NoHeader ForeignComment $ findHeader config source
    candidate = HeaderInfo fileType config detection mempty

commentSource :: [Text] -> SourceCode
commentSource = SourceCode . fmap (Comment,)

code :: Text -> SourceCode
code text = SourceCode [(Code, text)]

sourceTexts :: SourceCode -> [Text]
sourceTexts (SourceCode lines') = fmap snd lines'
