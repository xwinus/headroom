{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Config.Parse
-- Description : Structured configuration parsing errors
-- Copyright   : (c) 2019-2023 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Headroom.Config.Parse
    ( ConfigurationScope (..)
    , ConfigurationParseError (..)
    , decodeConfiguration
    )
where

import Data.Aeson (FromJSON)
import qualified Data.Yaml as Y
import Headroom.Types
    ( fromHeadroomError
    , toHeadroomError
    )
import RIO

-- | Identifies the configuration being parsed.
data ConfigurationScope
    = ProjectConfiguration
    | GlobalConfiguration
    deriving (Eq, Show)

-- | Error raised when YAML cannot be decoded into a configuration value.
data ConfigurationParseError
    = ConfigurationParseError
        ConfigurationScope
        Y.ParseException
    deriving (Show)

instance Exception ConfigurationParseError where
    displayException = displayException'
    toException = toHeadroomError
    fromException = fromHeadroomError

-- | Decodes YAML and converts parser failures into the Headroom error hierarchy.
decodeConfiguration
    :: (FromJSON a, MonadThrow m)
    => ConfigurationScope
    -> ByteString
    -> m a
decodeConfiguration scope =
    either (throwM . ConfigurationParseError scope) pure . Y.decodeEither'

displayException' :: ConfigurationParseError -> String
displayException' (ConfigurationParseError scope parseError) =
    "Cannot parse "
        <> scopeName scope
        <> " configuration:\n"
        <> Y.prettyPrintParseException parseError
  where
    scopeName = \case
        ProjectConfiguration -> "project"
        GlobalConfiguration -> "global"
