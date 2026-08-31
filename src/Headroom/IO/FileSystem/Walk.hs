{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.IO.FileSystem.Walk
-- Description : Pruning and deduplicating file-system walker
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Traverses source trees while pruning excluded directories and emitting each
-- canonical file at most once.
module Headroom.IO.FileSystem.Walk
    ( WalkPathKind (..)
    , WalkOptions (..)
    , WalkResult (..)
    , WalkFilesFn
    , walkFiles
    )
where

import RIO
import qualified RIO.Directory as D
import RIO.FilePath ((</>))
import qualified RIO.List as L
import qualified RIO.Set as S

-- | Options controlling which paths are traversed and emitted.
data WalkOptions m = WalkOptions
    { woSourcePaths :: [FilePath]
    , woIncludeFile :: FilePath -> Bool
    , woExcludePath :: FilePath -> Bool
    , woIgnorePath :: WalkPathKind -> FilePath -> m Bool
    }

-- | Filesystem path kind exposed to pruning predicates.
data WalkPathKind
    = WalkFile
    | WalkDirectory
    | WalkSymbolicLink
    deriving (Eq, Show)

-- | Summary of a completed walk.
data WalkResult = WalkResult
    { wrFilesFound :: Int
    , wrDirectoriesPruned :: Int
    , wrFileSystemErrors :: Int
    }
    deriving (Eq, Show)

-- | Type of a callback-based source walker.
type WalkFilesFn m = WalkOptions m -> (FilePath -> m ()) -> m WalkResult

data WalkState = WalkState
    { wsCanonicalDirectories :: Set FilePath
    , wsCanonicalFiles :: Set FilePath
    , wsFilesFound :: Int
    , wsDirectoriesPruned :: Int
    , wsFileSystemErrors :: Int
    }

data PathType
    = RegularDirectory
    | DirectorySymlink
    | FileSymlink
    | RegularFile
    | MissingPath

-- | Walks source paths in deterministic order, pruning directories before
-- descending and emitting each canonical file at most once.
walkFiles :: (MonadIO m) => WalkFilesFn m
walkFiles options emit = toResult <$> foldM walkPath initialState sourcePaths
  where
    sourcePaths = L.sort . L.nub $ woSourcePaths options

    walkPath state path =
        classifyPath path >>= \case
            Left _ -> pure $ recordFileSystemError state
            Right RegularDirectory -> walkDirectory state path
            Right DirectorySymlink -> pure $ recordPrunedDirectory state
            Right FileSymlink -> walkFile WalkSymbolicLink state path
            Right RegularFile -> walkFile WalkFile state path
            Right MissingPath -> pure $ recordFileSystemError state

    walkDirectory state path = do
        excluded <- shouldExclude WalkDirectory path
        if excluded
            then pure $ recordPrunedDirectory state
            else withCanonicalPath state path $ \canonical currentState ->
                if canonical `S.member` wsCanonicalDirectories currentState
                    then pure $ recordPrunedDirectory currentState
                    else do
                        let visited =
                                currentState
                                    { wsCanonicalDirectories =
                                        S.insert canonical
                                            $ wsCanonicalDirectories currentState
                                    }
                        children <- liftIO . tryIO $ D.listDirectory path
                        case children of
                            Left _ -> pure $ recordFileSystemError visited
                            Right names ->
                                foldM walkPath visited
                                    . fmap (path </>)
                                    $ L.sort names

    walkFile kind state path
        | not $ woIncludeFile options path = pure state
        | otherwise = do
            excluded <- shouldExclude kind path
            if excluded
                then pure state
                else withCanonicalPath state path $ \canonical currentState ->
                    if canonical `S.member` wsCanonicalFiles currentState
                        then pure currentState
                        else do
                            emit path
                            pure
                                currentState
                                    { wsCanonicalFiles =
                                        S.insert canonical
                                            $ wsCanonicalFiles currentState
                                    , wsFilesFound = wsFilesFound currentState + 1
                                    }

    shouldExclude kind path =
        if woExcludePath options path
            then pure True
            else woIgnorePath options kind path

    withCanonicalPath state path fn = do
        result <- liftIO . tryIO $ D.canonicalizePath path
        case result of
            Left _ -> pure $ recordFileSystemError state
            Right canonical -> fn canonical state

    classifyPath path = liftIO . tryIO $ do
        symbolicLink <- D.pathIsSymbolicLink path
        directory <- D.doesDirectoryExist path
        file <- D.doesFileExist path
        pure $ case (symbolicLink, directory, file) of
            (True, True, _) -> DirectorySymlink
            (True, _, True) -> FileSymlink
            (_, True, _) -> RegularDirectory
            (_, _, True) -> RegularFile
            _ -> MissingPath

    initialState = WalkState S.empty S.empty 0 0 0
    recordPrunedDirectory state =
        state{wsDirectoriesPruned = wsDirectoriesPruned state + 1}
    recordFileSystemError state =
        state{wsFileSystemErrors = wsFileSystemErrors state + 1}
    toResult state =
        WalkResult
            { wrFilesFound = wsFilesFound state
            , wrDirectoriesPruned = wsDirectoriesPruned state
            , wrFileSystemErrors = wsFileSystemErrors state
            }
