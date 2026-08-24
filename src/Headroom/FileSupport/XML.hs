{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileSupport.XML
-- Description : Support for /XML/ source code files
-- Copyright   : (c) 2019-2023 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Basic support for /XML/ source code files. This implementation doesn't
-- extract any variables or template data.
module Headroom.FileSupport.XML
    ( fileSupport
    )
where

import Headroom.Data.Regex
    ( isMatch
    , re
    )
import Headroom.FileSupport.Types
    ( FileSupport (..)
    , SyntaxAnalysis (..)
    , defaultFileSupport
    )
import Headroom.FileType.Types (FileType (XML))

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Implementation of 'FileSupport' for /XML/.
fileSupport :: FileSupport
fileSupport = defaultFileSupport XML syntaxAnalysis

------------------------------  PRIVATE FUNCTIONS  -----------------------------

syntaxAnalysis :: SyntaxAnalysis
syntaxAnalysis =
    SyntaxAnalysis
        { saIsCommentStart = isMatch [re|^<!--|]
        , saIsCommentEnd = isMatch [re|-->|]
        }
