{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.FileSupport.NixSpec
    ( spec
    )
where

import Headroom.Config
    ( makeHeaderConfig
    , makeHeadersConfig
    , parseAppConfig
    )
import Headroom.Config.Types
    ( AppConfig (..)
    , ConfigurationError (..)
    , ConfigurationKey (..)
    , HeadersConfig (..)
    , LicenseType (..)
    )
import Headroom.Data.Text (fromLines)
import Headroom.Embedded
    ( defaultConfig
    , licenseTemplate
    )
import Headroom.FileSupport
    ( analyzeSourceCode
    )
import Headroom.FileSupport.Nix
import Headroom.FileSupport.Types
    ( FileSupport (..)
    , SyntaxAnalysis (..)
    )
import Headroom.FileType.Types
    ( FileType (Nix)
    )
import Headroom.Header
    ( addHeader
    , findHeader
    )
import Headroom.Header.Types (HeaderInfo (..))
import Headroom.IO.FileSystem (loadFile)
import Headroom.SourceCode
    ( LineType (..)
    , SourceCode (..)
    )
import RIO
import RIO.FilePath ((</>))
import qualified RIO.Text as T
import Test.Hspec

spec :: Spec
spec = do
    describe "fsSyntaxAnalysis" $ do
        it "detects line comments without treating nix-shell directives as comments" $ do
            let samples =
                    [ ("# line comment", (True, True))
                    , ("#!/usr/bin/env nix-shell", (False, False))
                    , ("#! nix-shell -i bash", (False, False))
                    , ("not # a line comment", (False, False))
                    ]
            all checkSyntaxAnalysis samples `shouldBe` True

        it "detects block comment boundaries" $ do
            let samples =
                    [ ("/* block comment start", (True, False))
                    , ("block comment end */", (False, True))
                    , ("/* complete block comment */", (True, True))
                    , ("not /* a block comment", (False, False))
                    ]
            all checkSyntaxAnalysis samples `shouldBe` True

    describe "Nix source support" $ do
        it "provides a Nix template for every built-in license" $ do
            forM_ [Apache2, BSD3, GPL2, GPL3, MIT, MPL2] $ \license ->
                T.lines (licenseTemplate license Nix :: Text)
                    `shouldSatisfy` \ls -> not (null ls) && all ("#" `T.isPrefixOf`) ls

        it "analyzes line and block comments in Nix source" $ do
            sample <- loadFile $ "test-data" </> "code-samples" </> "nix" </> "sample1.nix"
            let expected =
                    SourceCode
                        [ (Code, "#!/usr/bin/env nix-shell")
                        , (Code, "#! nix-shell -i bash")
                        , (Code, "")
                        , (Comment, "# This is")
                        , (Comment, "# header")
                        , (Code, "")
                        , (Code, "{ pkgs ? import <nixpkgs> {} }:")
                        , (Comment, "/* This is not the header. */")
                        , (Code, "pkgs.mkShell {")
                        , (Comment, "  # A regular comment.")
                        , (Code, "}")
                        , (Code, "")
                        ]
                analyzed = analyzeSourceCode fileSupport sample
            analyzed `shouldBe` expected

        it "finds the first header after nix-shell directives" $ do
            config <- loadNixConfig
            sample <- loadFile $ "test-data" </> "code-samples" </> "nix" </> "sample1.nix"
            findHeader config (analyzeSourceCode fileSupport sample)
                `shouldBe` Just (3, 4)

        it "adds a header after all nix-shell directives" $ do
            config <- loadNixConfig
            let source =
                    analyzeSourceCode fileSupport
                        . fromLines
                        $ [ "#!/usr/bin/env nix-shell"
                          , "#! nix-shell -i bash"
                          , "{ pkgs }: pkgs.hello"
                          ]
                info = HeaderInfo Nix config Nothing mempty
                expected =
                    SourceCode
                        [ (Code, "#!/usr/bin/env nix-shell")
                        , (Code, "#! nix-shell -i bash")
                        , (Code, "")
                        , (Comment, "# Header")
                        , (Code, "")
                        , (Code, "{ pkgs }: pkgs.hello")
                        ]
            addHeader info "# Header" source `shouldBe` expected

        it "reports missing required Nix configuration" $ do
            config <- parseAppConfig incompleteNixConfig
            makeHeaderConfig Nix (hscNix $ acLicenseHeaders config)
                `shouldThrow` (== MissingConfiguration (CkMarginTopCode Nix))
  where
    checkSyntaxAnalysis (line, (starts, ends)) =
        let SyntaxAnalysis{..} = fsSyntaxAnalysis fileSupport
         in saIsCommentStart line == starts && saIsCommentEnd line == ends

    loadNixConfig = do
        config <- parseAppConfig defaultConfig
        hscNix <$> makeHeadersConfig (acLicenseHeaders config)

    incompleteNixConfig =
        "license-headers:\n  nix:\n    file-extensions: [\"nix\"]\n"
