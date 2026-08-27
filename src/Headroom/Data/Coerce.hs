{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Data.Coerce
-- Description : Extra functionality for coercion
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides some extra functionality extending the "Data.Coerce"
-- module.
module Headroom.Data.Coerce
    ( coerce
    , inner
    )
where

import Data.Coerce
    ( Coercible
    , coerce
    )
import RIO

-- | Allows to map the coercible value. This might be useful for example to
-- change the value within @newtype@, without manually unwrapping and wrapping
-- the value.
--
-- >>> import qualified RIO.Text as T
-- >>> newtype Foo = Foo Text deriving (Eq, Show)
-- >>> inner T.toUpper (Foo "hello")
-- Foo "HELLO"
inner
    :: (Coercible a b)
    => (b -> b)
    -- ^ function to modify coerced value
    -> a
    -- ^ value to modify
    -> a
    -- ^ modified value
inner f = coerce . f . coerce
