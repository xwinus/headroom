{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Data.Serialization
-- Description : Various functions for data (de)serialization
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Module providing support for data (de)serialization, mainly from/to /JSON/
-- and /YAML/.
module Headroom.Data.Serialization
    ( -- * JSON/YAML Serialization
      aesonOptions
    , dropFieldPrefix
    , symbolCase

      -- * Pretty Printing
    , prettyPrintYAML
    )
where

import Data.Aeson
    ( Options
    , ToJSON (..)
    , defaultOptions
    , fieldLabelModifier
    )
import qualified Data.Yaml.Pretty as YP
import RIO
import qualified RIO.Char as C

-- | Custom /Aeson/ encoding options used for generic mapping between data
-- records and /JSON/ or /YAML/ values. Expects the fields in input to be
-- without the prefix and with words formated in /symbol case/
-- (example: record field @uUserName@, /JSON/ field @user-name@).
aesonOptions :: Options
aesonOptions =
    defaultOptions{fieldLabelModifier = symbolCase '-' . dropFieldPrefix}

-- | Drops prefix from camel-case text.
--
-- >>> dropFieldPrefix "xxHelloWorld"
-- "helloWorld"
dropFieldPrefix :: String -> String
dropFieldPrefix = \case
    (x : n : xs) | C.isUpper x && C.isUpper n -> x : n : xs
    (x : n : xs) | C.isUpper x -> C.toLower x : n : xs
    (_ : xs) -> dropFieldPrefix xs
    [] -> []

-- | Transforms camel-case text into text cased with given symbol.
--
-- >>> symbolCase '-' "fooBar"
-- "foo-bar"
symbolCase
    :: Char
    -- ^ word separator symbol
    -> String
    -- ^ input text
    -> String
    -- ^ processed text
symbolCase sym = \case
    [] -> []
    (x : xs)
        | C.isUpper x -> sym : C.toLower x : symbolCase sym xs
        | otherwise -> x : symbolCase sym xs

-- | Pretty prints given data as /YAML/.
prettyPrintYAML
    :: (ToJSON a)
    => a
    -- ^ data to pretty print
    -> Text
    -- ^ pretty printed /YAML/ output
prettyPrintYAML = decodeUtf8Lenient . YP.encodePretty prettyConfig
  where
    prettyConfig = YP.setConfCompare compare YP.defConfig
