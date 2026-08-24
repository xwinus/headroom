{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.IO.FileSystemSpec
    ( spec
    )
where

import Headroom.Data.Regex (re)
import Headroom.IO.FileSystem
import RIO
import qualified RIO.Directory as D
import RIO.FilePath ((</>))
import RIO.List (sort)
import qualified RIO.List as L
import System.IO.Error (userError)
import Test.Hspec

spec :: Spec
spec = do
    describe "fileExtension" $ do
        it "returns file extension for valid file path" $ do
            fileExtension "/some/path/to/file.txt" `shouldBe` Just "txt"

        it "returns nothing for invalid file path" $ do
            fileExtension "/some/nonsense/path" `shouldBe` Nothing

    describe "findFiles" $ do
        it "recursively finds files filtered by given predicate" $ do
            let path = "test-data" </> "test-traverse"
                predicate = ("b.html" `L.isSuffixOf`)
                expected = ["test-data" </> "test-traverse" </> "foo" </> "b.html"]
            sort <$> findFiles path predicate `shouldReturn` sort expected

    describe "findFilesByExts" $ do
        it "recursively finds files filtered by its file extension" $ do
            let path = "test-data" </> "test-traverse"
                exts = ["xml"]
                expected = ["test-data" </> "test-traverse" </> "foo" </> "test.xml"]
            sort <$> findFilesByExts path exts `shouldReturn` sort expected

    describe "listFiles" $ do
        it "recursively finds all files in directory" $ do
            let path = "test-data" </> "test-traverse"
                expected =
                    [ "test-data" </> "test-traverse" </> "a.html"
                    , "test-data" </> "test-traverse" </> "foo" </> "b.html"
                    , "test-data" </> "test-traverse" </> "foo" </> "test.xml"
                    , "test-data" </> "test-traverse" </> "foo" </> "bar" </> "c.html"
                    ]
            sort <$> listFiles path `shouldReturn` sort expected

        it "returns file if file path is passed as argument" $ do
            let path = "test-data" </> "test-traverse" </> "a.html"
            sort <$> listFiles path `shouldReturn` [path]

    describe "excludePaths" $ do
        it "excludes paths matching selected pattern from input list" $ do
            let patterns = [[re|\.stack-work|], [re|remove\.txt|]]
                sample =
                    [ "/foo/bar/.stack-work/xx"
                    , "/hello/world"
                    , "foo/bar/remove.txt"
                    , "xx/yy"
                    ]
                expected = ["/hello/world", "xx/yy"]
            excludePaths patterns sample `shouldBe` expected

    describe "atomicWriteFile" $ do
        it "atomically replaces content and preserves permissions"
            . withSystemTempDirectory "atomic-write"
            $ \directory -> do
                let path = directory </> "source.hs"
                    fileSystem = mkFileSystem :: FileSystem IO
                writeFileUtf8 path "original"
                originalPermissions <- D.getPermissions path

                result <- atomicWriteFile fileSystem path "original" "updated"

                result `shouldBe` AtomicWriteSuccess
                readFileUtf8 path `shouldReturn` "updated"
                D.getPermissions path `shouldReturn` originalPermissions
                D.listDirectory directory `shouldReturn` ["source.hs"]

        it "preserves the original and cleans temporary files when rename fails"
            . withSystemTempDirectory "atomic-write"
            $ \directory -> do
                let path = directory </> "source.hs"
                    baseFileSystem = mkFileSystem :: FileSystem IO
                    fileSystem =
                        baseFileSystem
                            { fsRenameFile = \_ _ ->
                                throwIO $ userError "simulated rename failure"
                            }
                writeFileUtf8 path "original"

                atomicWriteFile fileSystem path "original" "updated"
                    `shouldThrow` anyIOException

                readFileUtf8 path `shouldReturn` "original"
                D.listDirectory directory `shouldReturn` ["source.hs"]

        it "preserves a concurrent change and reports a conflict"
            . withSystemTempDirectory "atomic-write"
            $ \directory -> do
                let path = directory </> "source.hs"
                    baseFileSystem = mkFileSystem :: FileSystem IO
                    fileSystem =
                        baseFileSystem
                            { fsLoadFile = \target -> do
                                writeFileUtf8 target "concurrent update"
                                pure "concurrent update"
                            }
                writeFileUtf8 path "original"

                result <- atomicWriteFile fileSystem path "original" "updated"

                result `shouldBe` AtomicWriteConflict
                readFileUtf8 path `shouldReturn` "concurrent update"
                D.listDirectory directory `shouldReturn` ["source.hs"]
