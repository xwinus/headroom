{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.HeaderSpec
    ( spec
    )
where

import Headroom.Config
    ( makeHeadersConfig
    , parseAppConfig
    )
import Headroom.Config.Types
    ( AppConfig (..)
    , HeaderConfig (..)
    , HeaderSyntax (..)
    , HeadersConfig (..)
    )
import Headroom.Data.Regex (re)
import Headroom.Embedded (defaultConfig)
import Headroom.FileSupport
    ( analyzeSourceCode
    , fileSupport
    )
import Headroom.FileType.Types (FileType (..))
import Headroom.Header
import Headroom.Header.Types
    ( HeaderDetection (..)
    , HeaderInfo (..)
    , HeaderOrigin (..)
    )
import Headroom.IO.FileSystem (loadFile)
import Headroom.SourceCode
    ( LineType (..)
    , SourceCode (..)
    )
import RIO
import RIO.FilePath ((</>))
import Test.Hspec hiding
    ( after
    , before
    )

spec :: Spec
spec = do
    let samplesDir = "test-data" </> "code-samples"
        lHeaderConfig pb pa =
            HeaderConfig ["hs"] 0 0 0 0 pb pa (LineComment [re|^--|] Nothing)
        bHeaderConfig = bHeaderConfigM 0 0 0 0
        bHeaderConfigM mtc mtf mbc mbf pb pa =
            HeaderConfig
                ["hs"]
                mtc
                mtf
                mbc
                mbf
                pb
                pa
                (BlockComment [re|^{-\||] [re|(?<!#)-}$|] Nothing)

    describe "addHeader" $ do
        let headerInfo config = HeaderInfo Haskell config NoHeader mempty

        it "adds header at the beginning of text (with no margin)" $ do
            let info = headerInfo $ bHeaderConfig [] []
                header = "HEADER"
                sample =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Code, "after")
                        , (Code, "4")
                        ]
                expected =
                    SourceCode
                        [ (Comment, "HEADER")
                        , (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Code, "after")
                        , (Code, "4")
                        ]
            addHeader info header sample `shouldBe` expected

        it "adds header at the beginning of text (with margin-top-file)" $ do
            let info = headerInfo $ bHeaderConfigM 0 1 0 0 [] []
                header = "HEADER"
                sample =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Code, "after")
                        , (Code, "4")
                        ]
                expected =
                    SourceCode
                        [ (Code, "")
                        , (Comment, "HEADER")
                        , (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Code, "after")
                        , (Code, "4")
                        ]
            addHeader info header sample `shouldBe` expected

        it "adds header at the beginning of text (with both margins)" $ do
            let info = headerInfo $ bHeaderConfigM 2 1 2 1 [] []
                header = "HEADER"
                sample =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Code, "after")
                        , (Code, "4")
                        ]
                expected =
                    SourceCode
                        [ (Code, "")
                        , (Comment, "HEADER")
                        , (Code, "")
                        , (Code, "")
                        , (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Code, "after")
                        , (Code, "4")
                        ]
            addHeader info header sample `shouldBe` expected

        it "adds header at correct position" $ do
            let info = headerInfo $ bHeaderConfig [[re|^before|]] [[re|^after|]]
                header = "{-| HEADER -}"
                sample =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Code, "after")
                        , (Code, "4")
                        ]
                expected =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Comment, "{-| HEADER -}")
                        , (Code, "after")
                        , (Code, "4")
                        ]
            addHeader info header sample `shouldBe` expected

        it "adds header at correct position (with both margins)" $ do
            let info =
                    headerInfo $ bHeaderConfigM 2 1 2 1 [[re|^before|]] [[re|^after|]]
                header = "{-| HEADER -}"
                sample =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Code, "after")
                        , (Code, "4")
                        ]
                expected =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Code, "")
                        , (Code, "")
                        , (Comment, "{-| HEADER -}")
                        , (Code, "")
                        , (Code, "")
                        , (Code, "after")
                        , (Code, "4")
                        ]
            addHeader info header sample `shouldBe` expected

        it "adds header at the end of text (with no margin)" $ do
            let info = headerInfo $ bHeaderConfig [[re|^before|]] []
                header = "{-| HEADER -}"
                sample = SourceCode [(Code, "1"), (Code, "2"), (Code, "before")]
                expected =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Comment, "{-| HEADER -}")
                        ]
            addHeader info header sample `shouldBe` expected

        it "adds header at the end of text (with both margins)" $ do
            let info = headerInfo $ bHeaderConfigM 2 1 2 1 [[re|^before|]] []
                header = "{-| HEADER -}"
                sample = SourceCode [(Code, "1"), (Code, "2"), (Code, "before")]
                expected =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Code, "")
                        , (Code, "")
                        , (Comment, "{-| HEADER -}")
                        , (Code, "")
                        ]
            addHeader info header sample `shouldBe` expected

        it "does nothing if header is already present" $ do
            let info =
                    HeaderInfo
                        Haskell
                        config
                        (ManagedHeader LegacyTemplate (3, 3))
                        mempty
                config = bHeaderConfig [[re|^before|]] []
                header = "{-| HEADER -}"
                sample =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Comment, "{-| OLDHEADER -}")
                        , (Code, "after")
                        , (Code, "4")
                        ]
            addHeader info header sample `shouldBe` sample

    describe "dropHeader" $ do
        it "does nothing if no header is present" $ do
            let config = bHeaderConfig [] []
                info = HeaderInfo Haskell config NoHeader mempty
                sample =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Code, "after")
                        , (Code, "4")
                        ]
            dropHeader info sample `shouldBe` sample

        it "drops existing single line header" $ do
            let config = bHeaderConfig [] []
                info =
                    HeaderInfo
                        Haskell
                        config
                        (ManagedHeader LegacyTemplate (3, 3))
                        mempty
                sample =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Comment, "{-| HEADER -}")
                        , (Code, "after")
                        , (Code, "4")
                        ]
                expected =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Code, "after")
                        , (Code, "4")
                        ]
            dropHeader info sample `shouldBe` expected

        it "drops existing multi line header" $ do
            let config = bHeaderConfig [] []
                info =
                    HeaderInfo
                        Haskell
                        config
                        (ManagedHeader LegacyTemplate (3, 4))
                        mempty
                sample =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Comment, "{-| HEADER")
                        , (Comment, "HERE -}")
                        , (Code, "after")
                        , (Code, "4")
                        ]
                expected =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Code, "after")
                        , (Code, "4")
                        ]
            dropHeader info sample `shouldBe` expected

    describe "replaceHeader" $ do
        it "replaces existing header" $ do
            let info =
                    HeaderInfo
                        Haskell
                        config
                        (ManagedHeader LegacyTemplate (3, 3))
                        mempty
                config = bHeaderConfig [[re|^before|]] []
                header = "{-| HEADER -}"
                sample =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Comment, "{-| OLDHEADER -}")
                        , (Code, "after")
                        , (Code, "4")
                        ]
                expected =
                    SourceCode
                        [ (Code, "1")
                        , (Code, "2")
                        , (Code, "before")
                        , (Comment, "{-| HEADER -}")
                        , (Code, "after")
                        , (Code, "4")
                        ]
            replaceHeader info header sample `shouldBe` expected

    describe "findHeader" $ do
        it "finds block header (one line long)" $ do
            let sample =
                    SourceCode
                        [ (Code, "")
                        , (Comment, "{-| single line -}")
                        , (Code, "")
                        , (Code, "")
                        ]
                config = bHeaderConfig [] []
            findHeader config sample `shouldBe` Just (1, 1)

        it "finds line header (one line long)" $ do
            let sample =
                    SourceCode
                        [(Code, ""), (Comment, "-- single line"), (Code, ""), (Code, "")]
                config = lHeaderConfig [] []
            findHeader config sample `shouldBe` Just (1, 1)

        it "finds block comment header put after 'putAfter' constraint" $ do
            let sample =
                    SourceCode
                        [ (Comment, "{-| 1 -}")
                        , (Code, "foo")
                        , (Comment, "{-| 2")
                        , (Comment, "2 -}")
                        , (Code, "bar")
                        , (Comment, "{-| 3")
                        , (Comment, "3 -}")
                        ]
                config = bHeaderConfig [[re|^foo|]] []
            findHeader config sample `shouldBe` Just (2, 3)

        it "finds line comment header put after 'putAfter' constraint" $ do
            let sample =
                    SourceCode
                        [ (Comment, "-- 1")
                        , (Code, "foo")
                        , (Comment, "-- 2")
                        , (Comment, "-- 2")
                        , (Code, "bar")
                        , (Comment, "-- 3")
                        , (Comment, "-- 3")
                        ]
                config = lHeaderConfig [[re|^foo|]] []
            findHeader config sample `shouldBe` Just (2, 3)

        it "finds block comment header put after composed constraint" $ do
            let sample =
                    SourceCode
                        [ (Comment, "{-| 1 -}")
                        , (Code, "foo")
                        , (Comment, "{-| 2")
                        , (Comment, "2 -}")
                        , (Code, "bar")
                        , (Comment, "{-| 3")
                        , (Comment, "3 -}")
                        ]
                config = bHeaderConfig [[re|^bar|^foo|]] []
            findHeader config sample `shouldBe` Just (5, 6)

        it "finds line comment header put after composed constraint" $ do
            let sample =
                    SourceCode
                        [ (Comment, "-- 1")
                        , (Code, "foo")
                        , (Comment, "-- 2")
                        , (Comment, "-- 2")
                        , (Code, "bar")
                        , (Comment, "-- 3")
                        , (Comment, "-- 3")
                        ]
                config = lHeaderConfig [[re|^bar|^foo|]] []
            findHeader config sample `shouldBe` Just (5, 6)

        it "finds nothing if no header present" $ do
            let sample = SourceCode [(Code, "some"), (Code, "code without header")]
                config = bHeaderConfig [] []
            findHeader config sample `shouldBe` Nothing

        it "finds nothing if header is present before 'putAfter' constraint" $ do
            let sample =
                    SourceCode
                        [ (Code, "foo")
                        , (Comment, "{-| 1 -}")
                        , (Code, "bar")
                        , (Code, "some text")
                        ]
                config = bHeaderConfig [[re|^bar|]] []
            findHeader config sample `shouldBe` Nothing

        it "correctly detects headers using default YAML configuration" $ do
            let loadSample = \ft p ->
                    analyzeSourceCode (fileSupport ft) <$> loadFile (samplesDir </> p)
            defaultConfig' <- parseAppConfig defaultConfig
            HeadersConfig{..} <- makeHeadersConfig (acLicenseHeaders defaultConfig')
            sampleC1 <- loadSample C $ "c" </> "sample1.c"
            sampleC2 <- loadSample C $ "c" </> "sample2.c"
            sampleCpp1 <- loadSample CPP $ "cpp" </> "sample1.cpp"
            sampleCpp2 <- loadSample CPP $ "cpp" </> "sample2.cpp"
            sampleCss1 <- loadSample CSS $ "css" </> "sample1.css"
            sampleCss2 <- loadSample CSS $ "css" </> "sample2.css"
            sampleDart <- loadSample Dart $ "dart" </> "sample1.dart"
            sampleGo <- loadSample Go $ "go" </> "sample1.go"
            sampleHs1 <- loadSample Haskell $ "haskell" </> "sample1.hs"
            sampleHs2 <- loadSample Haskell $ "haskell" </> "sample2.hs"
            sampleHtml1 <- loadSample HTML $ "html" </> "sample1.html"
            sampleHtml2 <- loadSample HTML $ "html" </> "sample2.html"
            sampleJava1 <- loadSample Java $ "java" </> "sample1.java"
            sampleJava2 <- loadSample Java $ "java" </> "sample2.java"
            sampleJs1 <- loadSample JS $ "js" </> "sample1.js"
            sampleKotlin <- loadSample Kotlin $ "kotlin" </> "sample1.kt"
            samplePhp1 <- loadSample PHP $ "php" </> "sample1.php"
            samplePureScript1 <- loadSample PureScript $ "purescript" </> "full.purs"
            samplePython1 <- loadSample Python $ "python" </> "sample1.py"
            sampleRust1 <- loadSample Rust $ "rust" </> "sample1.rs"
            sampleScala1 <- loadSample Scala $ "scala" </> "sample1.scala"
            sampleScala2 <- loadSample Scala $ "scala" </> "sample2.scala"
            sampleShell1 <- loadSample Shell $ "shell" </> "sample1.sh"
            sampleXml1 <- loadSample XML $ "xml" </> "sample1.xml"
            sampleXml2 <- loadSample XML $ "xml" </> "sample2.xml"
            findHeader hscC sampleC1 `shouldBe` Just (1, 3)
            findHeader hscC sampleC2 `shouldBe` Nothing
            findHeader hscCpp sampleCpp1 `shouldBe` Just (1, 3)
            findHeader hscCpp sampleCpp2 `shouldBe` Nothing
            findHeader hscCss sampleCss1 `shouldBe` Just (1, 4)
            findHeader hscCss sampleCss2 `shouldBe` Nothing
            findHeader hscDart sampleDart `shouldBe` Just (1, 2)
            findHeader hscGo sampleGo `shouldBe` Just (1, 2)
            findHeader hscHaskell sampleHs1 `shouldBe` Just (1, 3)
            findHeader hscHaskell sampleHs2 `shouldBe` Nothing
            findHeader hscHtml sampleHtml1 `shouldBe` Just (1, 4)
            findHeader hscHtml sampleHtml2 `shouldBe` Nothing
            findHeader hscJava sampleJava1 `shouldBe` Just (0, 2)
            findHeader hscJava sampleJava2 `shouldBe` Nothing
            findHeader hscJs sampleJs1 `shouldBe` Just (0, 2)
            findHeader hscKotlin sampleKotlin `shouldBe` Just (0, 2)
            findHeader hscPhp samplePhp1 `shouldBe` Just (2, 8)
            findHeader hscPureScript samplePureScript1 `shouldBe` Just (1, 1)
            findHeader hscPython samplePython1 `shouldBe` Just (2, 3)
            findHeader hscRust sampleRust1 `shouldBe` Just (0, 2)
            findHeader hscScala sampleScala1 `shouldBe` Just (0, 2)
            findHeader hscScala sampleScala2 `shouldBe` Nothing
            findHeader hscShell sampleShell1 `shouldBe` Just (2, 3)
            findHeader hscXml sampleXml1 `shouldBe` Just (2, 5)
            findHeader hscXml sampleXml2 `shouldBe` Nothing

    describe "findBlockHeader" $ do
        let s = [re|^{-\||]
            e = [re|(?<!#)-}$|]

        it "finds single line header" $ do
            let sample =
                    SourceCode
                        [ (Code, "")
                        , (Comment, "{-| single line -}")
                        , (Code, "")
                        , (Code, "")
                        ]
            findBlockHeader s e sample 0 `shouldBe` Just (1, 1)

        it "finds multi line header" $ do
            let sample =
                    SourceCode
                        [ (Code, "")
                        , (Comment, "{-| multi")
                        , (Comment, "line -}")
                        , (Code, "")
                        , (Code, "")
                        ]
            findBlockHeader s e sample 0 `shouldBe` Just (1, 2)

        it "finds only the first occurence of header" $ do
            let sample =
                    SourceCode
                        [ (Comment, "{-| this")
                        , (Comment, "and this -}")
                        , (Code, "")
                        , (Comment, "{-| no this -}")
                        ]
            findBlockHeader s e sample 0 `shouldBe` Just (0, 1)

        it "finds nothing if no header is present" $ do
            let sample = SourceCode [(Code, "foo"), (Code, "bar")]
            findBlockHeader s e sample 0 `shouldBe` Nothing

    describe "findLineHeader" $ do
        it "finds single line header" $ do
            let sample = SourceCode [(Comment, "-- foo"), (Code, "other")]
            findLineHeader [re|^--|] sample 0 `shouldBe` Just (0, 0)

        it "finds single line header with nothing surrounding it" $ do
            let sample = SourceCode [(Comment, "-- foo")]
            findLineHeader [re|^--|] sample 0 `shouldBe` Just (0, 0)

        it "finds multi line header with nothing surrounding it" $ do
            let sample = SourceCode [(Comment, "-- 3"), (Comment, "-- 3")]
            findLineHeader [re|^--|] sample 0 `shouldBe` Just (0, 1)

        it "finds multi line header with added offset" $ do
            let sample = SourceCode [(Comment, "-- 3"), (Comment, "-- 3")]
            findLineHeader [re|^--|] sample 2 `shouldBe` Just (2, 3)

        it "finds multi line header" $ do
            let sample =
                    SourceCode
                        [ (Code, "")
                        , (Code, "a")
                        , (Comment, "-- first")
                        , (Comment, "--second")
                        , (Code, "foo")
                        ]
            findLineHeader [re|^--|] sample 0 `shouldBe` Just (2, 3)

        it "finds only the first occurence of header" $ do
            let sample =
                    SourceCode
                        [ (Code, "a")
                        , (Comment, "-- this one")
                        , (Comment, "-- and this")
                        , (Code, "")
                        , (Comment, "-- not this")
                        ]
            findLineHeader [re|^--|] sample 0 `shouldBe` Just (1, 2)

        it "finds nothing if no header is present" $ do
            let sample = SourceCode [(Code, "foo"), (Code, "bar")]
            findLineHeader [re|^--|] sample 0 `shouldBe` Nothing

    describe "splitSource" $ do
        let sample =
                SourceCode
                    [ (Code, "some code")
                    , (Comment, "->")
                    , (Code, "->")
                    , (Code, "RESULT")
                    , (Comment, "<-")
                    , (Code, "<-")
                    , (Code, "also some code")
                    ]
            fstSplit = [[re|->|]]
            sndSplit = [[re|<-|]]

        it "handles empty source code and conditions" $ do
            splitSource [] [] mempty `shouldBe` (mempty, mempty, mempty)

        it "handles source code and empty conditions" $ do
            splitSource [] [] sample `shouldBe` (mempty, sample, mempty)

        it "splits source code with 1st split condition" $ do
            let before =
                    SourceCode [(Code, "some code"), (Comment, "->"), (Code, "->")]
                middle =
                    SourceCode
                        [ (Code, "RESULT")
                        , (Comment, "<-")
                        , (Code, "<-")
                        , (Code, "also some code")
                        ]
                after = mempty
                expected = (before, middle, after)
            splitSource fstSplit [] sample `shouldBe` expected

        it "splits source code with 2nd split condition" $ do
            let before = mempty
                middle =
                    SourceCode
                        [ (Code, "some code")
                        , (Comment, "->")
                        , (Code, "->")
                        , (Code, "RESULT")
                        , (Comment, "<-")
                        ]
                after = SourceCode [(Code, "<-"), (Code, "also some code")]
                expected = (before, middle, after)
            splitSource [] sndSplit sample `shouldBe` expected

        it "splits source code with both conditions" $ do
            let before =
                    SourceCode [(Code, "some code"), (Comment, "->"), (Code, "->")]
                middle = SourceCode [(Code, "RESULT"), (Comment, "<-")]
                after = SourceCode [(Code, "<-"), (Code, "also some code")]
                expected = (before, middle, after)
            splitSource fstSplit sndSplit sample `shouldBe` expected

        it "splits source code when nothing matches the 1st split condition" $ do
            let sample' =
                    SourceCode
                        [ (Code, "some code")
                        , (Comment, "->")
                        , (Code, "RESULT")
                        , (Comment, "<-")
                        , (Code, "<-")
                        , (Code, "also some code")
                        ]
                expected = (mempty, sample', mempty)
            splitSource fstSplit [] sample' `shouldBe` expected

        it "splits source code when nothing matches the 2nd split condition" $ do
            let sample' =
                    SourceCode
                        [ (Code, "some code")
                        , (Comment, "->")
                        , (Code, "->")
                        , (Code, "RESULT")
                        , (Comment, "<-")
                        , (Code, "also some code")
                        ]
                expected = (mempty, sample', mempty)
            splitSource [] sndSplit sample' `shouldBe` expected

        it "splits source code when nothing matches both conditions" $ do
            let sample' =
                    SourceCode
                        [ (Code, "some code")
                        , (Comment, "->")
                        , (Code, "RESULT")
                        , (Comment, "<-")
                        , (Code, "also some code")
                        ]
                expected = (mempty, sample', mempty)
            splitSource fstSplit sndSplit sample' `shouldBe` expected

        it "handles case when 2nd split is found before 1st split" $ do
            let before = mempty
                middle = SourceCode [(Code, "some code"), (Comment, "->")]
                after =
                    SourceCode
                        [ (Code, "->")
                        , (Code, "RESULT")
                        , (Comment, "<-")
                        , (Code, "<-")
                        , (Code, "also some code")
                        ]
                expected = (before, middle, after)
            splitSource sndSplit fstSplit sample `shouldBe` expected

        it "handles case when 1st split is also after 2nd split" $ do
            let sample' =
                    SourceCode
                        [ (Code, "some code")
                        , (Comment, "->")
                        , (Code, "->")
                        , (Code, "RESULT")
                        , (Comment, "<-")
                        , (Code, "<-")
                        , (Code, "->")
                        , (Code, "also some code")
                        ]
                before =
                    SourceCode [(Code, "some code"), (Comment, "->"), (Code, "->")]
                middle = SourceCode [(Code, "RESULT"), (Comment, "<-")]
                after =
                    SourceCode [(Code, "<-"), (Code, "->"), (Code, "also some code")]
                expected = (before, middle, after)
            splitSource fstSplit sndSplit sample' `shouldBe` expected
