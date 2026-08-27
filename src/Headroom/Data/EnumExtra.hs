{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Data.EnumExtra
-- Description : Extra functionality for enum types
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Provides extra functionality for enum-like types, e.g. reading/writing
-- from/to textual representation, etc.
module Headroom.Data.EnumExtra
    ( EnumExtra (..)
    )
where

import RIO
import qualified RIO.List as L
import qualified RIO.Text as T

-- | Enum data type, capable to (de)serialize itself from/to string
-- representation. Can be automatically derived by /GHC/ using the
-- @DeriveAnyClass@ extension.
class (Bounded a, Enum a, Eq a, Ord a, Show a) => EnumExtra a where
    -- | Returns list of all enum values.
    --
    -- >>> data Test = Foo | Bar deriving (Bounded, Enum, EnumExtra, Eq, Ord, Show)
    -- >>> allValues @Test
    -- [Foo,Bar]
    allValues :: [a]
    allValues = [minBound ..]

    -- | Returns all values of enum as single string, individual values separated
    -- with comma.
    --
    -- >>> data Test = Foo | Bar deriving (Bounded, Enum, EnumExtra, Eq, Ord, Show)
    -- >>> allValuesToText @Test
    -- "Foo, Bar"
    allValuesToText :: Text
    allValuesToText = T.intercalate ", " (fmap enumToText (allValues :: [a]))

    -- | Returns textual representation of enum value. Opposite to 'textToEnum'.
    --
    -- >>> data Test = Foo | Bar deriving (Bounded, Enum, EnumExtra, Eq, Ord, Show)
    -- >>> enumToText Bar
    -- "Bar"
    enumToText :: a -> Text
    enumToText = tshow

    -- | Returns enum value from its textual representation.
    -- Opposite to 'enumToText'.
    --
    -- >>> data Test = Foo | Bar deriving (Bounded, Enum, EnumExtra, Eq, Ord, Show)
    -- >>> (textToEnum "Foo") :: (Maybe Test)
    -- Just Foo
    textToEnum :: Text -> Maybe a
    textToEnum text =
        let enumValue v = (T.toLower . enumToText $ v) == T.toLower text
         in L.find enumValue allValues
