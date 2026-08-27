{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileSupport.TemplateData
-- Description : Custom data specific to file support implementations
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Because each implementation of file support might require some custom data
-- extracted for example from the template file, this module exposes data types
-- specific for individual implementations.
module Headroom.FileSupport.TemplateData
    ( TemplateData (..)
    , HaddockOffsets (..)
    , HaskellTemplateData' (..)
    )
where

import RIO

-- | Additional template data extracted from the template file.
data TemplateData
    = -- | additional template data for /Haskell/
      HaskellTemplateData HaskellTemplateData'
    | -- | no additional template data provided
      NoTemplateData
    deriving (Eq, Show)

-- | Offsets for selected fields extracted from /Haddock module header/.
data HaddockOffsets = HaddockOffsets
    { hoCopyright :: Maybe Int
    -- ^ offset for /Copyright/ field
    }
    deriving (Eq, Show)

-- | Additional template data required by /Haskell/ file support
data HaskellTemplateData' = HaskellTemplateData'
    { htdHaddockOffsets :: HaddockOffsets
    -- ^ offsets for /Haddock/ fields
    }
    deriving (Eq, Show)
