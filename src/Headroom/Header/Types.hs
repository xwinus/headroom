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
      HeaderInfo (..)
    , HeaderTemplate (..)
    )
where

import Headroom.Config.Types (CtHeaderConfig)
import Headroom.FileSupport.TemplateData (TemplateData)
import Headroom.FileType.Types (FileType)
import Headroom.Meta (TemplateType)
import Headroom.Variables.Types (Variables)
import RIO

-- | Info extracted about the source code file header.
data HeaderInfo = HeaderInfo
    { hiFileType :: FileType
    -- ^ type of the file
    , hiHeaderConfig :: CtHeaderConfig
    -- ^ configuration for license header
    , hiHeaderPos :: Maybe (Int, Int)
    -- ^ position of existing license header
    , hiVariables :: Variables
    -- ^ additional extracted variables
    }
    deriving (Eq, Show)

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
