{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Command.Run.Action
-- Description : Source-file action selection
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Chooses the source transformation and user-facing messages for each run mode
-- and detected header state.
module Headroom.Command.Run.Action
    ( RunAction (..)
    , chooseAction
    )
where

import Headroom.Config.Types
    ( AppConfig (..)
    , CtAppConfig
    , RunMode (..)
    )
import Headroom.Data.Has (Has (..))
import Headroom.Header
    ( addHeader
    , dropHeader
    , replaceHeader
    )
import Headroom.Header.Types
    ( HeaderDetection (..)
    , HeaderInfo (..)
    )
import Headroom.SourceCode (SourceCode)
import RIO
import qualified RIO.Text as T

-- | Action to be performed based on the selected 'RunMode'.
data RunAction = RunAction
    { raProcessed :: Bool
    , raFunc :: SourceCode -> SourceCode
    , raProcessedMsg :: Text
    , raSkippedMsg :: Text
    }

-- | Chooses a source transformation for the configured run mode.
chooseAction :: (Has CtAppConfig env) => HeaderInfo -> Text -> RIO env RunAction
chooseAction info header = do
    AppConfig{..} <- viewL @CtAppConfig
    pure (go acRunMode $ hiHeaderDetection info)
  where
    go runMode detection = case runMode of
        Add -> addAction detection
        Check -> checkAction detection
        Drop -> dropAction detection
        Replace -> replaceAction detection
    addAction detection =
        RunAction
            (not $ isManaged detection)
            (addHeader info header)
            (justify "Adding header to:")
            (justify "Header already exists in:")
    checkAction detection =
        (checkBase detection)
            { raProcessedMsg = justify "Outdated header found in:"
            , raSkippedMsg = justify "Header up-to-date in:"
            }
    checkBase detection@ForeignComment{} = addAction detection
    checkBase detection = replaceAction detection
    dropAction detection = case detection of
        ManagedHeader{} ->
            RunAction
                True
                (dropHeader info)
                (justify "Dropping header from:")
                (justify "No header exists in:")
        ForeignComment{} -> foreignAction
        NoHeader ->
            RunAction
                False
                id
                (justify "Dropping header from:")
                (justify "No header exists in:")
    replaceAction detection = case detection of
        ManagedHeader{} -> replaceManagedAction
        NoHeader -> go Add detection
        ForeignComment{} -> foreignAction
    replaceManagedAction =
        RunAction
            True
            (replaceHeader info header)
            (justify "Replacing header in:")
            (justify "Header up-to-date in:")
    foreignAction =
        RunAction
            False
            id
            (justify "Skipping foreign comment in:")
            (justify "Foreign comment preserved in:")
    isManaged ManagedHeader{} = True
    isManaged _ = False
    justify = T.justifyLeft 30 ' '
