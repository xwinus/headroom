{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.PostProcess.UpdateCopyright
-- Description : /Post-processor/ for updating years in copyrights
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides functionality for updating years in copyright statements
-- in already rendered /license headers/.
module Headroom.PostProcess.UpdateCopyright
    ( -- * Data Types
      SelectedAuthors (..)
    , UpdateCopyrightMode (..)

      -- * Header Functions
    , updateCopyright

      -- * Helper Functions
    , updateYears
    )
where

import Data.String.Interpolate (i)
import Headroom.Data.Has (Has (..))
import Headroom.Data.Regex
    ( match
    , re
    , scan
    )
import Headroom.Data.Text
    ( mapLines
    , read
    )
import Headroom.PostProcess.Types (PostProcess (..))
import Headroom.Types (CurrentYear (..))
import RIO
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

---------------------------------  DATA TYPES  ---------------------------------

-- | Non-empty list of authors for which to update years in their copyrights.
newtype SelectedAuthors = SelectedAuthors (NonEmpty Text) deriving (Eq, Show)

-- | Mode that changes behaviour of the 'updateCopyright' function.
data UpdateCopyrightMode
    = -- | updates years in copyrights for all authors
      UpdateAllAuthors
    | -- | updates years in copyrights only for selected authors
      UpdateSelectedAuthors SelectedAuthors
    deriving (Eq, Show)

-- | Parsed parts of a single copyright statement.
data CopyrightStatement = CopyrightStatement
    { csPrefix :: Text
    , csStartYear :: Integer
    , csEndYear :: Maybe Integer
    , csAuthorText :: Text
    }
    deriving (Eq, Show)

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | /Post-processor/ that updates years and year ranges in any
-- present copyright statements.
--
-- = Reader Environment Parameters
--   ['CurrentYear'] value of the current year
--   ['UpdateCopyrightMode'] mode specifying the behaviour of the updater
updateCopyright
    :: (Has CurrentYear env, Has UpdateCopyrightMode env)
    => PostProcess env
updateCopyright = PostProcess $ \input -> do
    currentYear <- viewL
    mode <- viewL
    pure $ mapLines (update mode currentYear) input
  where
    update mode year line = case parseCopyrightStatement line of
        Just statement
            | shouldUpdate mode statement ->
                renderCopyrightStatement year statement
        _ -> line
    shouldUpdate UpdateAllAuthors _ = True
    shouldUpdate (UpdateSelectedAuthors (SelectedAuthors authors)) statement =
        any (`T.isInfixOf` csAuthorText statement) (NE.toList authors)

-- | Updates years and years ranges in given text.
--
-- >>> updateYears (CurrentYear 2020) "Copyright (c) 2020"
-- "Copyright (c) 2020"
--
-- >>> updateYears (CurrentYear 2020) "Copyright (c) 2019"
-- "Copyright (c) 2019-2020"
--
-- >>> updateYears (CurrentYear 2020) "Copyright (c) 2018-2020"
-- "Copyright (c) 2018-2020"
--
-- >>> updateYears (CurrentYear 2020) "Copyright (c) 2018-2019"
-- "Copyright (c) 2018-2020"
updateYears
    :: CurrentYear
    -- ^ current year
    -> Text
    -- ^ text to update
    -> Text
    -- ^ text with updated years
updateYears cy = mapLines updateLine
  where
    updateLine line = case parseCopyrightStatement line of
        Just statement -> renderCopyrightStatement cy statement
        Nothing -> line

------------------------------  PRIVATE FUNCTIONS  -----------------------------

parseCopyrightStatement :: Text -> Maybe CopyrightStatement
parseCopyrightStatement input = do
    guard . isSingleCopyright $ input
    case match statementPattern input of
        Just [_, prefix, rawStartYear, rawEndYear, authorText] -> do
            startYear <- read rawStartYear
            endYear <- parseEndYear rawEndYear
            guard $ maybe True (startYear <=) endYear
            guard . not $ hasAmbiguousLeadingYear authorText
            pure $ CopyrightStatement prefix startYear endYear authorText
        _ -> Nothing
  where
    statementPattern =
        [re|^([^\r\n]*?\b(?i:copyright)\b(?:\h+|\h*:\h*)(?:(?:\([cC]\)|©)\h*)?)([12]\d{3})(?:-([12]\d{3}))?((?:\h+.*)?)$|]
    isSingleCopyright text =
        length (scan [re|\b(?i:copyright)\b|] text) == 1
    parseEndYear "" = Just Nothing
    parseEndYear raw = Just <$> read raw
    hasAmbiguousLeadingYear author =
        isJust $ match [re|^\h+\d{4}(?:\b|-)|] author

renderCopyrightStatement :: CurrentYear -> CopyrightStatement -> Text
renderCopyrightStatement cy (CopyrightStatement prefix startYear endYear author) =
    prefix <> updatedYears <> author
  where
    updatedYears = case endYear of
        Just end -> bumpRange cy startYear end
        Nothing -> bumpYear cy startYear

bumpYear :: CurrentYear -> Integer -> Text
bumpYear (CurrentYear cy) y
    | y >= cy = tshow y
    | otherwise = [i|#{y}-#{cy}|]

bumpRange :: CurrentYear -> Integer -> Integer -> Text
bumpRange (CurrentYear cy) y1 y2
    | y2 >= cy = [i|#{y1}-#{y2}|]
    | otherwise = [i|#{y1}-#{cy}|]
