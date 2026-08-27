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
    ( re
    , replace
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
    update mode year line
        | shouldUpdate mode line = updateYears year line
        | otherwise = line
    shouldUpdate UpdateAllAuthors _ = True
    shouldUpdate (UpdateSelectedAuthors (SelectedAuthors authors)) input =
        any (`T.isInfixOf` input) (NE.toList authors)

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
updateYears cy = replace [re|(\d{4})(?:-)?(\d{4})?|] go
  where
    go _ [r1] | (Just y1) <- read r1 = bumpYear cy y1
    go _ rs@[_, _] | [Just y1, Just y2] <- read <$> rs = bumpRange cy y1 y2
    go other _ = other

------------------------------  PRIVATE FUNCTIONS  -----------------------------

bumpYear :: CurrentYear -> Integer -> Text
bumpYear (CurrentYear cy) y
    | y >= cy = tshow y
    | otherwise = [i|#{y}-#{cy}|]

bumpRange :: CurrentYear -> Integer -> Integer -> Text
bumpRange (CurrentYear cy) y1 y2
    | y2 >= cy = [i|#{y1}-#{y2}|]
    | otherwise = [i|#{y1}-#{cy}|]
