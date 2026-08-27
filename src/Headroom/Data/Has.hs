{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Data.Has
-- Description : Simplified variant of @Data.Has@
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides 'Has' /type class/, adapted to the needs of this
-- application.
module Headroom.Data.Has
    ( Has (..)
    , HasRIO
    )
where

import RIO

-- | Implementation of the /Has type class/ pattern.
class Has a t where
    {-# MINIMAL getter, modifier | hasLens #-}

    getter :: t -> a
    getter = getConst . hasLens Const

    modifier :: (a -> a) -> t -> t
    modifier f t = runIdentity (hasLens (Identity . f) t)

    hasLens :: Lens' t a
    hasLens afa t = (\a -> modifier (const a) t) <$> afa (getter t)

    viewL :: (MonadReader t m) => m a
    viewL = view hasLens

-- | Handy type alias that allows to avoid ugly type singatures. Allows to
-- transform this:
--
-- @
-- foo :: (Has (Network (RIO env)) env) => RIO env ()
-- @
--
-- into
--
-- @
-- foo :: (HasRIO Network env) => RIO env ()
-- @
type HasRIO a env = Has (a (RIO env)) env
