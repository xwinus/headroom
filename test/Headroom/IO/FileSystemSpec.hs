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
import RIO.FilePath
    ( takeFileName
    , (</>)
    )
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

    describe "walkFiles" $ do
        it "discovers relevant files under multiple roots deterministically"
            . withSystemTempDirectory "source-walk"
            $ \directory -> do
                let firstRoot = directory </> "first"
                    secondRoot = directory </> "second"
                    firstSource = firstRoot </> "Main.hs"
                    secondSource = secondRoot </> "Nested" </> "Util.hs"
                    ignoredExtension = secondRoot </> "README.md"
                    options = defaultWalkOptions [secondRoot, firstRoot]
                    expected = [firstSource, secondSource]
                D.createDirectoryIfMissing True $ secondRoot </> "Nested"
                D.createDirectoryIfMissing True firstRoot
                writeFileUtf8 firstSource "module Main where"
                writeFileUtf8 secondSource "module Util where"
                writeFileUtf8 ignoredExtension "documentation"

                (firstResult, firstPaths) <- collectWalk options
                (secondResult, secondPaths) <- collectWalk options

                firstResult `shouldBe` WalkResult 2 0 0
                secondResult `shouldBe` firstResult
                firstPaths `shouldBe` expected
                secondPaths `shouldBe` expected

        it "deduplicates overlapping roots and duplicate explicit files"
            . withSystemTempDirectory "source-walk"
            $ \directory -> do
                let sourceRoot = directory </> "src"
                    sourcePath = sourceRoot </> "Main.hs"
                    aliasPath = directory </> "Alias.hs"
                    options =
                        defaultWalkOptions
                            [directory, sourceRoot, sourcePath, aliasPath, directory]
                D.createDirectoryIfMissing True sourceRoot
                writeFileUtf8 sourcePath "module Main where"
                D.createFileLink sourcePath aliasPath
                checkedRef <- newIORef []
                let recordKind kind path = do
                        modifyIORef' checkedRef (<> [(kind, path)])
                        pure False

                (result, paths) <-
                    collectWalk options{woIgnorePath = recordKind}
                checked <- readIORef checkedRef

                wrFilesFound result `shouldBe` 1
                paths `shouldBe` [aliasPath]
                checked `shouldContain` [(WalkSymbolicLink, aliasPath)]
                checked `shouldContain` [(WalkFile, sourcePath)]

        it "prunes ignored directories before visiting their children"
            . withSystemTempDirectory "source-walk"
            $ \directory -> do
                let sourceRoot = directory </> "src"
                    ignoredRoot = sourceRoot </> "ignored"
                    visibleSource = sourceRoot </> "Visible.hs"
                    hiddenSource = ignoredRoot </> "deep" </> "Hidden.hs"
                D.createDirectoryIfMissing True $ ignoredRoot </> "deep"
                writeFileUtf8 visibleSource "module Visible where"
                writeFileUtf8 hiddenSource "module Hidden where"
                checkedRef <- newIORef []
                let ignorePath kind path = do
                        modifyIORef' checkedRef (<> [(kind, path)])
                        pure $ takeFileName path == "ignored"
                    options =
                        (defaultWalkOptions [sourceRoot]){woIgnorePath = ignorePath}

                (result, paths) <- collectWalk options
                checked <- readIORef checkedRef

                paths `shouldBe` [visibleSource]
                wrDirectoriesPruned result `shouldBe` 1
                checked `shouldContain` [(WalkDirectory, ignoredRoot)]
                fmap snd checked `shouldNotContain` [hiddenSource]

        it "terminates safely when a directory symlink creates a cycle"
            . withSystemTempDirectory "source-walk"
            $ \directory -> do
                let sourceRoot = directory </> "src"
                    sourcePath = sourceRoot </> "Main.hs"
                    cyclePath = sourceRoot </> "cycle"
                D.createDirectoryIfMissing True sourceRoot
                writeFileUtf8 sourcePath "module Main where"
                D.createDirectoryLink sourceRoot cyclePath

                (result, paths) <- collectWalk $ defaultWalkOptions [sourceRoot]

                paths `shouldBe` [sourcePath]
                wrDirectoriesPruned result `shouldBe` 1
                wrFileSystemErrors result `shouldBe` 0

        it "reports a missing source path without aborting the walk"
            . withSystemTempDirectory "source-walk"
            $ \directory -> do
                let missingPath = directory </> "missing"

                (result, paths) <- collectWalk $ defaultWalkOptions [missingPath]

                paths `shouldBe` []
                result `shouldBe` WalkResult 0 0 1

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

defaultWalkOptions :: [FilePath] -> WalkOptions IO
defaultWalkOptions sourcePaths =
    WalkOptions
        { woSourcePaths = sourcePaths
        , woIncludeFile = (".hs" `L.isSuffixOf`)
        , woExcludePath = const False
        , woIgnorePath = \_ _ -> pure False
        }

collectWalk :: WalkOptions IO -> IO (WalkResult, [FilePath])
collectWalk options = do
    pathsRef <- newIORef []
    result <- walkFiles options $ \path -> modifyIORef' pathsRef (<> [path])
    paths <- readIORef pathsRef
    pure (result, paths)
