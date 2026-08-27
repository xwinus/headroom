{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileSupport.Shell
-- Description : Support for /Shell/ source code files
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Basic support for /Shell/ source code files. This implementation doesn't extract
-- any variables or template data.
module Headroom.FileSupport.Shell
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
import Headroom.FileType.Types (FileType (Shell))

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Implementation of 'FileSupport' for /Shell/.
fileSupport :: FileSupport
fileSupport = defaultFileSupport Shell syntaxAnalysis

------------------------------  PRIVATE FUNCTIONS  -----------------------------

syntaxAnalysis :: SyntaxAnalysis
syntaxAnalysis =
    SyntaxAnalysis
        { saIsCommentStart = isMatch [re|^#(?!!)|]
        , saIsCommentEnd = isMatch [re|^#(?!!)|]
        }
