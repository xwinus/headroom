{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileSupport.Java
-- Description : Support for /Java/ source code files
-- Copyright   : (c) 2019-2023 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Support for /Java/ source code files. This implementation extracts /Java/
-- package name as variable.
--
-- = Extracted Variables for Templates
-- This implementation extracts following variables from source code file:
--
-- * @___java_package_name__@ - name of the /Java/ package
--
-- = Extracted Custom Data
-- This implementation does not extract any custom data from template file.
module Headroom.FileSupport.Java
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
import Headroom.FileType.Types (FileType (Java))
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

-- | Implementation of 'FileSupport' for /Java/.
fileSupport :: FileSupport
fileSupport =
    FileSupport
        { fsSyntaxAnalysis = syntaxAnalysis
        , fsExtractTemplateData = const . const $ NoTemplateData
        , fsExtractVariables = extractVariables
        , fsFileType = Java
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
        [("_java_package_name",) <$> extractPackageName source]

extractPackageName :: SourceCode -> Maybe Text
extractPackageName = fmap snd . firstMatching f
  where
    f (lt, l)
        | lt == Code = match [re|^package (.*);$|] l >>= (^? ix 1)
        | otherwise = Nothing
