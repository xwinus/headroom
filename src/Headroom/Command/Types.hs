{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Command.Types
-- Description : Data types for "Headroom.Command"
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains data types for "Headroom.Command" module.
module Headroom.Command.Types
    ( Command (..)
    , CommandGenOptions (..)
    , CommandInitOptions (..)
    , CommandRunOptions (..)
    )
where

import Headroom.Config.Types
    ( GenMode
    , LicenseType
    , RunMode
    )
import Headroom.Data.Regex (Regex)
import Headroom.FileType.Types (FileType)
import Headroom.Template.TemplateRef (TemplateRef)
import RIO

-- | Application command.
data Command
    = -- | @run@ command
      Run [FilePath] [Regex] Bool (Maybe LicenseType) [TemplateRef] [Text] (Maybe RunMode) Bool Bool
    | -- | @gen@ command
      Gen Bool (Maybe (LicenseType, FileType))
    | -- | @init@ command
      Init LicenseType [FilePath]
    deriving (Show)

-- | Options for the @gen@ command.
newtype CommandGenOptions = CommandGenOptions
    { cgoGenMode :: GenMode
    -- ^ selected mode
    }
    deriving (Show)

-- | Options for the @init@ command.
data CommandInitOptions = CommandInitOptions
    { cioSourcePaths :: [FilePath]
    -- ^ paths to source code files
    , cioLicenseType :: LicenseType
    -- ^ license type
    }
    deriving (Show)

-- | Options for the @run@ command.
data CommandRunOptions = CommandRunOptions
    { croRunMode :: Maybe RunMode
    -- ^ used /Run/ command mode
    , croSourcePaths :: [FilePath]
    -- ^ source code file paths
    , croExcludedPaths :: [Regex]
    -- ^ source paths to exclude
    , croExcludeIgnoredPaths :: Bool
    -- ^ whether to exclude ignored paths
    , croBuiltInTemplates :: Maybe LicenseType
    -- ^  whether to use built-in templates
    , croTemplateRefs :: [TemplateRef]
    -- ^ template references
    , croVariables :: [Text]
    -- ^ raw variables
    , croDebug :: Bool
    -- ^ whether to run in debug mode
    , croDryRun :: Bool
    -- ^ whether to perform dry run
    }
    deriving (Eq, Show)
