{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.UI.Progress
-- Description : UI component for displaying progress
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This component displays progress in format @[CURR of TOTAL]@.
module Headroom.UI.Progress
    ( Progress (..)
    , zipWithProgress
    )
where

import RIO
import qualified RIO.Text as T
import Text.Printf (printf)

-- | Progress indication with either a known total or only the current count.
data Progress
    = -- | Current progress and maximum value.
      Progress Int Int
    | -- | Current progress while discovering work incrementally.
      CurrentProgress Int
    deriving (Eq, Show)

instance Display Progress where
    textDisplay (Progress current total) =
        T.pack
            $ mconcat ["[", currentS, " of ", totalS, "]"]
      where
        format = "%" <> (show . length $ totalS) <> "d"
        currentS = printf format current
        totalS = show total
    textDisplay (CurrentProgress current) = T.pack $ "[" <> show current <> "]"

-- | Zips given list with the progress info.
--
-- >>> zipWithProgress ["a", "b"]
-- [(Progress 1 2,"a"),(Progress 2 2,"b")]
zipWithProgress
    :: [a]
    -- ^ list to zip with progress
    -> [(Progress, a)]
    -- ^ zipped result
zipWithProgress list = zip progresses list
  where
    listLength = length list
    progresses = fmap (`Progress` listLength) [1 .. listLength]
