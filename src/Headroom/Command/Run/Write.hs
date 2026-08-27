{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Command.Run.Write
-- Description : Safe persistence of transformed source files.
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Persists transformed source files while preserving their original file
-- permissions.
module Headroom.Command.Run.Write
    ( CommandRunError (..)
    , writeSourceFile
    )
where

import Data.String.Interpolate (iii)
import Headroom.Config.Types (RunMode (..))
import Headroom.Data.Has
    ( HasRIO
    , viewL
    )
import Headroom.IO.FileSystem
    ( AtomicWriteResult (..)
    , FileSystem
    , atomicWriteFile
    )
import Headroom.Types
    ( fromHeadroomError
    , toHeadroomError
    )
import RIO

-- | Writes changed source code unless the selected mode prohibits writes.
writeSourceFile
    :: (HasRIO FileSystem env)
    => Bool
    -> RunMode
    -> Bool
    -> FilePath
    -> Text
    -> Text
    -> RIO env ()
writeSourceFile dryRun runMode changed path originalContent newContent =
    when (not dryRun && runMode /= Check && changed) $ do
        fileSystem <- viewL
        atomicWriteFile fileSystem path originalContent newContent >>= \case
            AtomicWriteSuccess -> pure ()
            AtomicWriteConflict -> throwM $ SourceFileChanged path

-- | Exception specific to writing files in the run command.
data CommandRunError
    = SourceFileChanged FilePath
    deriving (Eq, Show)

instance Exception CommandRunError where
    displayException (SourceFileChanged path) =
        [iii|
      Source file '#{path}' changed while it was being processed.
      No changes were written; retry the command.
    |]
    toException = toHeadroomError
    fromException = fromHeadroomError
