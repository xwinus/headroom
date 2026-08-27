{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Variables.Types
-- Description : Data types for "Headroom.Variables"
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains data types for "Headroom.Variables" module.
module Headroom.Variables.Types
    ( Variables (..)
    )
where

import RIO

-- | Map of /static/ and /dynamic variables/. Use 'Headroom.Variables.mkVariables' function for
-- more convenient construction of this data type.
newtype Variables = Variables (HashMap Text Text) deriving (Eq, Show)

instance Semigroup Variables where
    (Variables x) <> (Variables y) = Variables (y <> x)

instance Monoid Variables where
    mempty = Variables mempty
