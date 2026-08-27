{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.UI.Table
-- Description : UI components for rendering tables
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Module providing UI components for tables.
module Headroom.UI.Table where

import qualified Headroom.Data.Text as T
import RIO
import qualified RIO.List.Partial as LP
import qualified RIO.Text as T

-- | Represents two columns wide table.
newtype Table2 = Table2 [(Text, Text)] deriving (Eq, Show)

instance Display Table2 where
    textDisplay (Table2 rows) =
        let maxWidth = (+ 1) . maximum' . fmap (T.length . fst) $ rows
            aligned = fmap (\(c1, c2) -> T.justifyLeft maxWidth ' ' c1 <> c2) rows
         in T.fromLines aligned
      where
        maximum' [] = 0
        maximum' xs = LP.maximum xs
