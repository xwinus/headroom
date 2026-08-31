{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Header.Types
-- Description : Data types for "Headroom.Header"
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains data types for "Headroom.Header" module.
module Headroom.Header.Types
    ( -- * Data Types
      HeaderDetection (..)
    , HeaderInfo (..)
    , HeaderOrigin (..)
    , HeaderPosition
    , HeaderTemplate (..)

      -- * Detection Helpers
    , candidateHeaderPosition
    , managedHeaderPosition
    )
where

import Headroom.Config.Types (CtHeaderConfig)
import Headroom.FileSupport.TemplateData (TemplateData)
import Headroom.FileType.Types (FileType)
import Headroom.Meta (TemplateType)
import Headroom.Variables.Types (Variables)
import RIO

-- | Inclusive zero-based position of a header in a source file.
type HeaderPosition = (Int, Int)

-- | Evidence used to establish ownership of a managed header.
data HeaderOrigin
    = HeadroomMarker
    | LegacyTemplate
    deriving (Eq, Show)

-- | Result of classifying the comment at the configured insertion point.
data HeaderDetection
    = ManagedHeader HeaderOrigin HeaderPosition
    | ForeignComment HeaderPosition
    | NoHeader
    deriving (Eq, Show)

-- | Info extracted about the source code file header.
data HeaderInfo = HeaderInfo
    { hiFileType :: FileType
    -- ^ type of the file
    , hiHeaderConfig :: CtHeaderConfig
    -- ^ configuration for license header
    , hiHeaderDetection :: HeaderDetection
    -- ^ ownership classification of the comment at the insertion point
    , hiVariables :: Variables
    -- ^ additional extracted variables
    }
    deriving (Eq, Show)

-- | Returns the syntactic comment candidate, regardless of its ownership.
candidateHeaderPosition :: HeaderDetection -> Maybe HeaderPosition
candidateHeaderPosition (ManagedHeader _ position) = Just position
candidateHeaderPosition (ForeignComment position) = Just position
candidateHeaderPosition NoHeader = Nothing

-- | Returns the position only when the header is known to be managed.
managedHeaderPosition :: HeaderDetection -> Maybe HeaderPosition
managedHeaderPosition (ManagedHeader _ position) = Just position
managedHeaderPosition _ = Nothing

-- | Represents info about concrete header template.
data HeaderTemplate = HeaderTemplate
    { htConfig :: CtHeaderConfig
    -- ^ header configuration
    , htTemplateData :: TemplateData
    -- ^ extra template data extracted by the correcponding file type support
    , htFileType :: FileType
    -- ^ type of the file this template is for
    , htTemplate :: TemplateType
    -- ^ parsed template
    }
    deriving (Eq, Show)
