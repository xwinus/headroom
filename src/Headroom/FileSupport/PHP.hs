{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileSupport.PHP
-- Description : Support for /PHP/ source code files
-- Copyright   : (c) 2019-2023 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Basic support for /PHP/ source code files. This implementation doesn't
-- extract any variables or template data.
module Headroom.FileSupport.PHP
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
import Headroom.FileType.Types (FileType (PHP))

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Implementation of 'FileSupport' for /Javascript/.
fileSupport :: FileSupport
fileSupport = defaultFileSupport PHP syntaxAnalysis

------------------------------  PRIVATE FUNCTIONS  -----------------------------

syntaxAnalysis :: SyntaxAnalysis
syntaxAnalysis =
    SyntaxAnalysis
        { saIsCommentStart = isMatch [re|^\/\*|^\/\/|]
        , saIsCommentEnd = isMatch [re|\*\/|^\/\/|]
        }
