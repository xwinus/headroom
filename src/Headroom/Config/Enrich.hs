{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Config.Enrich
-- Description : Simple enrichment of YAML configuration stubs
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains mini-DSL for enrichment of generated /YAML/ configurations,
-- i.e. replacing default values with real ones, etc. This is intentionally done
-- without the "Data.Yaml" and parsing, because that way all comments inside YAML
-- file would be lost.
module Headroom.Config.Enrich
    ( -- * Data Types
      Enrich (..)
    , ValueType (..)

      -- * Field Generators
    , withArray
    , withText

      -- * Field Manipulation
    , replaceEmptyValue
    )
where

import Data.Aeson (ToJSON (..))
import Headroom.Data.Serialization (prettyPrintYAML)
import RIO
import qualified RIO.Map as M
import qualified RIO.Text as T
import qualified RIO.Text.Partial as TP

---------------------------------  DATA TYPES  ---------------------------------

-- | Simple wrapper representing single step of enrichment.
newtype Enrich = Enrich
    { enrich :: Text -> Text
    -- ^ takes input text and does enrichment
    }

instance Semigroup Enrich where
    Enrich fnA <> Enrich fnB = Enrich $ fnA . fnB

instance Monoid Enrich where
    mempty = Enrich id

-- | Represents type of the field value.
data ValueType
    = -- | type of /YAML/ array
      Array
    | -- | type of /YAML/ string
      String
    deriving (Eq, Show)

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Generates /YAML/ array field from given list and field name.
withArray
    :: (ToJSON a)
    => [a]
    -- ^ input list used as value
    -> Text
    -- ^ field name
    -> (ValueType, Text)
    -- ^ generated fields as @(valueType, generatedField)@
withArray list field = (Array, asYAML field list)

-- | Generates /YAML/ string from given text value and field name.
withText
    :: Text
    -- ^ input text value
    -> Text
    -- ^ field name
    -> (ValueType, Text)
    -- ^ generated fields as @(valueType, generatedField)@
withText text field = (String, asYAML field text)

-- | Replaces empty value of given field with actual generated value.
replaceEmptyValue
    :: Text
    -- ^ field name
    -> (Text -> (ValueType, Text))
    -- ^ field value generator function
    -> Enrich
    -- ^ resulting enrichment step
replaceEmptyValue field replaceFn = Enrich $ \doc -> do
    TP.replace old new doc
  where
    (tpe, new) = replaceFn field
    old = field <> ": " <> emptyValue tpe

------------------------------  PRIVATE FUNCTIONS  -----------------------------

asYAML :: (ToJSON a) => Text -> a -> Text
asYAML field value = T.stripEnd . prettyPrintYAML $ M.fromList [(field, value)]

emptyValue :: ValueType -> Text
emptyValue Array = "[]"
emptyValue String = "\"\""
