{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Command.Readers
-- Description : Custom readers for /optparse-applicative/ library
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains custom readers required by the /optparse-applicative/
-- library to parse data types such as 'LicenseType' or 'FileType'.
module Headroom.Command.Readers
    ( licenseReader
    , licenseTypeReader
    , regexReader
    , templateRefReader
    , parseLicense
    )
where

import Data.Either.Combinators (maybeToRight)
import Headroom.Config.Types (LicenseType)
import Headroom.Data.EnumExtra (EnumExtra (..))
import Headroom.Data.Regex
    ( Regex (..)
    , compile
    )
import Headroom.FileType.Types (FileType (..))
import Headroom.Template.TemplateRef
    ( TemplateRef (..)
    , mkTemplateRef
    )
import Options.Applicative
import RIO
import qualified RIO.Text as T
import qualified RIO.Text.Partial as TP

-- | Reader for tuple of 'LicenseType' and 'FileType'.
licenseReader :: ReadM (LicenseType, FileType)
licenseReader = eitherReader parseLicense'
  where
    parseLicense' raw = maybeToRight errMsg (parseLicense $ T.pack raw)
    errMsg =
        T.unpack
            $ mconcat
                [ "invalid license/file type, must be in format 'licenseType:fileType' "
                , "(e.g. bsd3:haskell)"
                , "\nAvailable license types: "
                , T.toLower (allValuesToText @LicenseType)
                , "\nAvailable file types: "
                , T.toLower (allValuesToText @FileType)
                ]

-- | Reader for 'LicenseType'.
licenseTypeReader :: ReadM LicenseType
licenseTypeReader = eitherReader parseLicenseType
  where
    parseLicenseType raw = maybeToRight errMsg (textToEnum $ T.pack raw)
    errMsg =
        T.unpack
            $ mconcat
                [ "invalid license type, available options: "
                , T.toLower (allValuesToText @LicenseType)
                ]

-- | Reader for 'Regex'.
regexReader :: ReadM Regex
regexReader =
    let parse input = mapLeft displayException (compile . T.pack $ input)
     in eitherReader parse

-- | Reader for 'TemplateRef'.
templateRefReader :: ReadM TemplateRef
templateRefReader =
    let parse input = mapLeft displayException (mkTemplateRef . T.pack $ input)
     in eitherReader parse

-- | Parses 'LicenseType' and 'FileType' from the input string,
-- formatted as @licenseType:fileType@.
--
-- >>> parseLicense "bsd3:haskell"
-- Just (BSD3,Haskell)
parseLicense :: Text -> Maybe (LicenseType, FileType)
parseLicense raw
    | [lt, ft] <- TP.splitOn ":" raw = (,) <$> textToEnum lt <*> textToEnum ft
    | otherwise = Nothing
