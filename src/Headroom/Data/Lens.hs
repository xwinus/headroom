{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Data.Lens
-- Description : Custom functionality related to /lens/
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Provides customized versions of /lens/ functions.
module Headroom.Data.Lens
    ( suffixLenses
    , suffixLensesFor
    )
where

import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH
import Lens.Micro.TH
    ( DefName (..)
    , lensField
    , lensRules
    , lensRulesFor
    , makeLensesWith
    )
import RIO

-- | A template haskell function to build lenses for a record type. This
-- function differs from the 'Control.Lens.makeLenses' function in that
-- it does not require the record fields to be prefixed with underscores
-- and it adds an "L" suffix to lens names to make it clear that they
-- are lenses.
suffixLenses :: TH.Name -> TH.DecsQ
suffixLenses = makeLensesWith $ lensRules & lensField .~ withSuffix
  where
    withSuffix _ _ name = [TopName . TH.mkName $ (TH.nameBase name <> "L")]

-- | Same as 'suffixLensesFor', but build lenses only for selected fields.
suffixLensesFor :: [String] -> TH.Name -> TH.DecsQ
suffixLensesFor fields = makeLensesWith $ lensRulesFor fields'
  where
    fields' = fmap (\f -> (f, f <> "L")) fields
