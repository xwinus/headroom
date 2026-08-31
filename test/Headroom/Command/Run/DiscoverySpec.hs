{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Command.Run.DiscoverySpec
    ( spec
    )
where

import qualified Data.VCS.Ignore as VCS
import Headroom.Command.Run.Discovery (isRepositoryPathIgnored)
import Headroom.IO.FileSystem (WalkPathKind (..))
import RIO
import qualified RIO.Directory as D
import RIO.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = describe "isRepositoryPathIgnored" $ do
    it "applies root and nested ignore rules to walker paths"
        . withSystemTempDirectory "git-ignore"
        $ \repository -> do
            let sourceRoot = repository </> "src"
                ignoredDirectory = repository </> "ignored"
                nestedIgnored = sourceRoot </> "cache.tmp"
                visibleSource = sourceRoot </> "Main.hs"
            initializeRepository repository
            D.createDirectoryIfMissing True ignoredDirectory
            D.createDirectoryIfMissing True sourceRoot
            writeFileUtf8 (repository </> ".gitignore") "ignored/"
            writeFileUtf8 (sourceRoot </> ".gitignore") "*.tmp"
            writeFileUtf8 nestedIgnored "temporary"
            writeFileUtf8 visibleSource "module Main where"
            matcher <- VCS.openGitRepository repository

            isRepositoryPathIgnored matcher WalkDirectory ignoredDirectory
                `shouldReturn` True
            D.withCurrentDirectory repository
                $ isRepositoryPathIgnored matcher WalkFile ("src" </> "cache.tmp")
                `shouldReturn` True
            isRepositoryPathIgnored matcher WalkFile visibleSource
                `shouldReturn` False

    it "does not apply repository rules to paths outside the repository"
        . withSystemTempDirectory "git-ignore"
        $ \parent -> do
            let repository = parent </> "repository"
                outside = parent </> "ignored"
            initializeRepository repository
            D.createDirectoryIfMissing True outside
            writeFileUtf8 (repository </> ".gitignore") "ignored/"
            matcher <- VCS.openGitRepository repository

            isRepositoryPathIgnored matcher WalkDirectory outside
                `shouldReturn` False

    it "matches a file symlink by its repository path"
        . withSystemTempDirectory "git-ignore"
        $ \parent -> do
            let repository = parent </> "repository"
                outside = parent </> "outside.hs"
                ignoredLink = repository </> "ignored.hs"
            initializeRepository repository
            writeFileUtf8 (repository </> ".gitignore") "ignored.hs"
            writeFileUtf8 outside "module Outside where"
            D.createFileLink outside ignoredLink
            matcher <- VCS.openGitRepository repository

            isRepositoryPathIgnored matcher WalkSymbolicLink ignoredLink
                `shouldReturn` True

    it "always excludes repository metadata"
        . withSystemTempDirectory "git-ignore"
        $ \repository -> do
            let metadata = repository </> ".git" </> "objects"
            initializeRepository repository
            D.createDirectoryIfMissing True metadata
            matcher <- VCS.openGitRepository repository

            isRepositoryPathIgnored matcher WalkDirectory metadata
                `shouldReturn` True

initializeRepository :: FilePath -> IO ()
initializeRepository repository =
    D.createDirectoryIfMissing True $ repository </> ".git" </> "info"
