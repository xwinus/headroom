{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileSupport.PureScript
-- Description : Support for /PureScript/ source code files
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Support for /PureScript/ source code files. This implementation extracts
-- /PureScript/ module name as variable.
--
-- = Extracted Variables for Templates
-- This implementation extracts following variables from source code file:
--
-- * @___purescript_module_name__@ - name of the /PureScript/ module
--
-- = Extracted Custom Data
-- This implementation does not extract any custom data from template file.
module Headroom.FileSupport.PureScript
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
import Headroom.FileType.Types (FileType (PureScript))
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

-- | Implementation of 'FileSupport' for /PureScript/.
fileSupport :: FileSupport
fileSupport =
    FileSupport
        { fsSyntaxAnalysis = syntaxAnalysis
        , fsExtractTemplateData = const . const $ NoTemplateData
        , fsExtractVariables = extractVariables
        , fsFileType = PureScript
        }

------------------------------  PRIVATE FUNCTIONS  -----------------------------

syntaxAnalysis :: SyntaxAnalysis
syntaxAnalysis =
    SyntaxAnalysis
        { saIsCommentStart = isMatch [re|^{-(?!\h*#)|^--|]
        , saIsCommentEnd = isMatch [re|(?<!#)-}|^--|]
        }

extractVariables
    :: HeaderTemplate
    -> Maybe (Int, Int)
    -> SourceCode
    -> Variables
extractVariables _ _ source =
    (mkVariables . catMaybes)
        [("_purescript_module_name",) <$> extractModuleName source]

extractModuleName :: SourceCode -> Maybe Text
extractModuleName = fmap snd . firstMatching f
  where
    f (lt, l)
        | lt == Code = match [re|^module\s+(\S+)|] l >>= (^? ix 1)
        | otherwise = Nothing
