{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileSupport.Kotlin
-- Description : Support for /Kotlin/ source code files
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Support for /Kotlin/ source code files. This implementation extracts /Kotlin/
-- package name as variable.
--
-- = Extracted Variables for Templates
-- This implementation extracts following variables from source code file:
--
-- * @___kotlin_package_name__@ - name of the /Kotlin/ package
--
-- = Extracted Custom Data
-- This implementation does not extract any custom data from template file.
module Headroom.FileSupport.Kotlin
    ( fileSupport
    )
where

import Headroom.Data.Regex
    ( isMatch
    , match
    , re
    )
import Headroom.FileSupport.TemplateData (TemplateData (..))
import Headroom.FileSupport.Types
    ( FileSupport (..)
    , SyntaxAnalysis (..)
    )
import Headroom.FileType.Types (FileType (Kotlin))
import Headroom.Header.Types (HeaderTemplate)
import Headroom.SourceCode
    ( LineType (..)
    , SourceCode (..)
    , firstMatching
    )
import Headroom.Variables (mkVariables)
import Headroom.Variables.Types (Variables (..))
import RIO
import RIO.Lens (ix)

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Implementation of 'FileSupport' for /Kotlin/.
fileSupport :: FileSupport
fileSupport =
    FileSupport
        { fsSyntaxAnalysis = syntaxAnalysis
        , fsExtractTemplateData = const . const $ NoTemplateData
        , fsExtractVariables = extractVariables
        , fsFileType = Kotlin
        }

------------------------------  PRIVATE FUNCTIONS  -----------------------------

syntaxAnalysis :: SyntaxAnalysis
syntaxAnalysis =
    SyntaxAnalysis
        { saIsCommentStart = isMatch [re|^\/\*|^\/\/|]
        , saIsCommentEnd = isMatch [re|\*\/|^\/\/|]
        }

extractVariables
    :: HeaderTemplate
    -> Maybe (Int, Int)
    -> SourceCode
    -> Variables
extractVariables _ _ source =
    (mkVariables . catMaybes)
        [("_kotlin_package_name",) <$> extractPackageName source]

extractPackageName :: SourceCode -> Maybe Text
extractPackageName = fmap snd . firstMatching f
  where
    f (lt, l)
        | lt == Code = match [re|^package (.*)$|] l >>= (^? ix 1)
        | otherwise = Nothing
