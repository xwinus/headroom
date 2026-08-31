{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Header.Marker
-- Description : Ownership markers for generated license headers
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Headroom.Header.Marker
    ( endMarker
    , isHeaderMarkerLine
    , markHeader
    , markerPosition
    , startMarker
    )
where

import Headroom.Config.Types (HeaderSyntax (..))
import qualified Headroom.Data.Regex as R
import Headroom.Header.Sanitize
    ( findPrefix
    , sanitizeSyntax
    , stripCommentSyntax
    )
import Headroom.Header.Types (HeaderPosition)
import RIO
import qualified RIO.List as L
import qualified RIO.Text as T

startMarker :: Text
startMarker = "headroom:managed:start:v1"

endMarker :: Text
endMarker = "headroom:managed:end:v1"

-- | Adds paired ownership markers without modifying the rendered header body.
markHeader :: HeaderSyntax -> Text -> Text
markHeader syntax header
    | hasMarker startMarker header && hasMarker endMarker header = header
    | otherwise = case syntax of
        LineComment{} -> withLineMarkers
        BlockComment start end _ -> fromMaybe header $ withBlockMarkers start end
  where
    withLineMarkers = T.intercalate "\n" [lineMarker startMarker, header, lineMarker endMarker]
    lineMarker = sanitizeSyntax (findPrefix syntax header)
    withBlockMarkers start end = do
        opening <- firstMatch start header
        closing <- lastMatch end header
        let marker value = T.intercalate " " [opening, value, closing]
        pure $ T.intercalate "\n" [marker startMarker, header, marker endMarker]

-- | Finds an exact range bounded by a single valid marker pair. The opening
-- marker must be the first line of the syntactic candidate.
markerPosition :: HeaderSyntax -> Int -> [Text] -> Maybe HeaderPosition
markerPosition syntax offset lines' = case (indices startMarker, indices endMarker) of
    ([0], [end]) | end > 0 -> Just (offset, offset + end)
    _ -> Nothing
  where
    indices marker =
        [ index
        | (index, line) <- zip [0 :: Int ..] lines'
        , T.strip (stripCommentSyntax markerSyntax line) == marker
        ]
    markerSyntax = case syntax of
        BlockComment start end _ -> BlockComment start end Nothing
        LineComment start _ -> LineComment start Nothing

isHeaderMarkerLine :: Text -> Bool
isHeaderMarkerLine line = hasMarker startMarker line || hasMarker endMarker line

hasMarker :: Text -> Text -> Bool
hasMarker = T.isInfixOf

firstMatch :: R.Regex -> Text -> Maybe Text
firstMatch regex = fmap fst . listToMaybe . R.scan regex

lastMatch :: R.Regex -> Text -> Maybe Text
lastMatch regex = fmap fst . L.lastMaybe . R.scan regex
