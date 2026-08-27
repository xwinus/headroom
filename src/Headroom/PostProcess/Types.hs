{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.PostProcess.Types
-- Description : Data types for /post-processing/
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains data types and /type class/ instances for the
-- /post-processing/ functions.
module Headroom.PostProcess.Types
    ( PostProcess (..)
    )
where

import RIO

-- | Definition of /post-processor/, i.e. function, that is applied to
-- already rendered /license header/, performs some logic and returns modified
-- text of /license header/. Given that the /reader monad/ and 'ReaderT'
-- transformer is used, any configuration is provided using the @env@
-- environment. When combined with the "Headroom.Data.Has" monad, it provides
-- powerful way how to combine different /post-processors/ and
-- environments.
--
-- = Structure of post-processor
--
-- @
-- __Text -> Reader env Text__
--   │              │   │
--   └─ rendered text of license header
--                  │   │
--                  └─ environment holding possible configuration
--                      │
--                      └─ modified license header text
-- @
newtype PostProcess env = PostProcess (Text -> Reader env Text)

instance Semigroup (PostProcess env) where
    PostProcess fnX <> PostProcess fnY = PostProcess $ fnX >=> fnY

instance Monoid (PostProcess env) where
    mempty = PostProcess $ \input -> pure input
