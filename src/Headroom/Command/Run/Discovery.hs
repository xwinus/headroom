{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Command.Run.Discovery
-- Description : Streaming source file discovery
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Connects project configuration and Git ignore rules to the pruning source
-- walker used by the run command.
module Headroom.Command.Run.Discovery
    ( walkSourceFiles
    , isRepositoryPathIgnored
    )
where

import Data.String.Interpolate (i)
import qualified Data.VCS.Ignore as VCS
import Headroom.Config.Types
    ( AppConfig (..)
    , CtAppConfig
    )
import Headroom.Data.Has
    ( Has (..)
    , HasRIO
    )
import Headroom.IO.FileSystem
    ( FileSystem (..)
    , WalkOptions (..)
    , WalkPathKind (..)
    , WalkResult
    , excludePaths
    )
import RIO
import qualified RIO.Directory as D
import RIO.FilePath
    ( isRelative
    , makeRelative
    , normalise
    , splitDirectories
    , takeDirectory
    , takeFileName
    , (</>)
    )

-- | Streams configured source files to the supplied callback.
walkSourceFiles
    :: (Has CtAppConfig env, HasRIO FileSystem env, HasLogFunc env)
    => (FilePath -> Bool)
    -> (FilePath -> RIO env ())
    -> RIO env WalkResult
walkSourceFiles includeFile emit = do
    AppConfig{..} <- viewL @CtAppConfig
    FileSystem{..} <- viewL
    ignorePath <- ignorePathPredicate
    logDebug $ "Using source paths: " <> displayShow acSourcePaths
    fsWalkFiles
        WalkOptions
            { woSourcePaths = acSourcePaths
            , woIncludeFile = includeFile
            , woExcludePath = \path ->
                null $ excludePaths acExcludedPaths [path]
            , woIgnorePath = ignorePath
            }
        emit

ignorePathPredicate
    :: (Has CtAppConfig env, HasRIO FileSystem env, HasLogFunc env)
    => RIO env (WalkPathKind -> FilePath -> RIO env Bool)
ignorePathPredicate = do
    AppConfig{..} <- viewL @CtAppConfig
    FileSystem{..} <- viewL
    if not acExcludeIgnoredPaths
        then pure $ \_ _ -> pure False
        else do
            currentDirectory <- fsGetCurrentDirectory
            findRepo currentDirectory
  where
    findRepo directory = do
        logInfo "Searching for VCS repository to extract exclude patterns from..."
        maybeRepo <- liftIO $ VCS.findGitRepository directory
        case maybeRepo of
            Just repository -> do
                logInfo [i|Found Git repository in: #{VCS.repositoryRoot repository}|]
                pure $ \kind path ->
                    liftIO $ isRepositoryPathIgnored repository kind path
            Nothing -> do
                logInfo [i|No VCS repository found in: #{directory}|]
                pure $ \_ _ -> pure False

-- | Checks a walker path using repository-relative Git ignore semantics.
isRepositoryPathIgnored
    :: VCS.GitRepository
    -> WalkPathKind
    -> FilePath
    -> IO Bool
isRepositoryPathIgnored repository kind path = do
    maybeRelative <- repositoryRelativePath repository path
    maybe (pure False) (VCS.isIgnored repository $ gitPathKind kind) maybeRelative

repositoryRelativePath :: VCS.GitRepository -> FilePath -> IO (Maybe FilePath)
repositoryRelativePath repository path = do
    absolute <- D.makeAbsolute path
    lexical <-
        if normalise absolute == root
            then pure root
            else do
                canonicalParent <- D.canonicalizePath $ takeDirectory absolute
                pure . normalise $ canonicalParent </> takeFileName absolute
    let relative = makeRelative root lexical
    pure $ if isOutsideRepository relative then Nothing else Just relative
  where
    root = VCS.repositoryRoot repository
    isOutsideRepository relative =
        not (isRelative relative) || case splitDirectories relative of
            ".." : _ -> True
            _ -> False

gitPathKind :: WalkPathKind -> VCS.PathKind
gitPathKind WalkFile = VCS.RegularFile
gitPathKind WalkDirectory = VCS.Directory
gitPathKind WalkSymbolicLink = VCS.SymbolicLink
