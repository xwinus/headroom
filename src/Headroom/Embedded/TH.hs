{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Embedded.TH
-- Description : /Template Haskell/ functions for "Headroom.Embedded"
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains some /Template Haskell/ powered functions, used by
-- "Headroom.Embedded" module, that needs to be placed in separated module, due to
-- /GHC/ stage restriction.
module Headroom.Embedded.TH
    ( embedConfigFile
    , embedDefaultConfig
    , embedDefaultGlobalConfig
    , embedTemplate
    )
where

import Data.FileEmbed (embedStringFile)
import Headroom.Config.Types (LicenseType (..))
import Headroom.Data.EnumExtra (EnumExtra (..))
import Headroom.FileType.Types (FileType (..))
import Headroom.Meta (TemplateType)
import Headroom.Template (Template (..))
import Language.Haskell.TH.Syntax
    ( Exp
    , Q
    )
import RIO
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

-- | Embeds stub configuration file to source code.
embedConfigFile :: Q Exp
embedConfigFile = embedStringFile "embedded/config-file.yaml"

-- | Embeds default configuration file to source code.
embedDefaultConfig :: Q Exp
embedDefaultConfig = embedStringFile "embedded/default-config.yaml"

-- | Embeds default global configuration file to source code.
embedDefaultGlobalConfig :: Q Exp
embedDefaultGlobalConfig =
    embedStringFile "embedded/default-global-config.yaml"

-- | Embeds /template file/ to the source code.
embedTemplate
    :: LicenseType
    -- ^ type of the /license/
    -> FileType
    -- ^ type of the source code file
    -> Q Exp
    -- ^ content of the appropriate /template/ file
embedTemplate lt ft =
    (embedStringFile . mconcat)
        ["embedded/license/", toStringLC lt, "/", toStringLC ft, ".", ext]
  where
    ext = T.unpack . NE.head $ templateExtensions @TemplateType

------------------------------  PRIVATE FUNCTIONS  -----------------------------

toStringLC :: (EnumExtra a) => a -> String
toStringLC = T.unpack . T.toLower . enumToText
