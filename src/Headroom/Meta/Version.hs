{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Meta.Version
-- Description : Type safe representation of Haskell PVP version
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains data types and functions for working with
-- Haskell PVP versions (<https://pvp.haskell.org/faq/>) in type safe way.
module Headroom.Meta.Version
    ( Version (..)
    , parseVersion
    , printVersion
    , printVersionP
    , pvp
    )
where

import Data.Aeson
    ( FromJSON (..)
    , withText
    )
import Headroom.Data.Regex
    ( match
    , re
    )
import qualified Headroom.Data.Text as T
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import RIO
import qualified RIO.Text as T

---------------------------------  DATA TYPES  ---------------------------------

-- | Type safe representation of /PVP/ version.
data Version = Version
    { vMajor1 :: Int
    -- ^ first major version
    , vMajor2 :: Int
    -- ^ second major version
    , vMinor :: Int
    -- ^ minor version
    , vPatch :: Int
    -- ^ patch level version
    }
    deriving (Eq, Show)

instance Ord Version where
    compare (Version a1 b1 c1 d1) (Version a2 b2 c2 d2) = go pairs
      where
        pairs = [(a1, a2), (b1, b2), (c1, c2), (d1, d2)]
        go [] = EQ
        go ((x, y) : xs)
            | x /= y = compare x y
            | otherwise = go xs

instance FromJSON Version where
    parseJSON = withText "Version" $ \raw ->
        maybe (fail . errorMsg $ raw) pure (parseVersion raw)

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Parses 'Version' from given text.
--
-- >>> parseVersion "0.3.2.0"
-- Just (Version {vMajor1 = 0, vMajor2 = 3, vMinor = 2, vPatch = 0})
parseVersion
    :: Text
    -- ^ input text to parse version from
    -> Maybe Version
    -- ^ parsed 'Version'
parseVersion raw = do
    groups <- match [re|^v?([0-9]+)\.([0-9]+)\.([0-9]+)\.([0-9]+)$|] raw
    check (mapMaybe T.read groups)
  where
    check [ma1, ma2, mi, p] = Just $ Version ma1 ma2 mi p
    check _ = Nothing

-- | Prints 'Version' in @major1.major2.minor.patch@ format.
--
-- >>> printVersion (Version 0 3 2 0)
-- "0.3.2.0"
printVersion
    :: Version
    -- ^ 'Version' to print
    -> Text
    -- ^ textual representation
printVersion (Version ma1 ma2 mi p) = T.intercalate "." chunks
  where
    chunks = tshow <$> [ma1, ma2, mi, p]

-- | Similar to 'printVersion', but adds the @v@ prefix in front of the version
-- number.
--
-- >>> printVersionP (Version 0 3 2 0)
-- "v0.3.2.0"
printVersionP :: Version -> Text
printVersionP = ("v" <>) . printVersion

-- | QuasiQuoter for defining 'Version' values checked at compile time.
--
-- >>> [pvp|1.2.3.4|]
-- Version {vMajor1 = 1, vMajor2 = 2, vMinor = 3, vPatch = 4}
pvp :: QuasiQuoter
pvp =
    QuasiQuoter
        { quoteExp = quoteExpVersion
        , quotePat = undefined
        , quoteType = undefined
        , quoteDec = undefined
        }
  where
    quoteExpVersion txt = [|parseVersionUnsafe . T.pack $ txt|]
      where
        !_ = parseVersionUnsafe . T.pack $ txt -- check at compile time

------------------------------  PRIVATE FUNCTIONS  -----------------------------

parseVersionUnsafe :: Text -> Version
parseVersionUnsafe raw = case parseVersion raw of
    Nothing -> error . errorMsg $ raw
    Just res -> res

errorMsg :: Text -> String
errorMsg raw =
    mconcat
        [ "Value '"
        , T.unpack raw
        , "' is not valid PVP version string. Please define correct version in "
        , "format 'MAJOR1.MAJOR2.MINOR.PATCH' (e.g. '0.4.1.2')."
        ]
