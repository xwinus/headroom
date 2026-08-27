{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileSupport.Python
-- Description : Support for /Python/ source code files
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Basic support for /Python/ source code files. This implementation doesn't
-- extract any variables or template data.
module Headroom.FileSupport.Python
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
import Headroom.FileType.Types (FileType (Python))

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Implementation of 'FileSupport' for /Python/.
fileSupport :: FileSupport
fileSupport = defaultFileSupport Python syntaxAnalysis

------------------------------  PRIVATE FUNCTIONS  -----------------------------

syntaxAnalysis :: SyntaxAnalysis
syntaxAnalysis =
    SyntaxAnalysis
        { saIsCommentStart = isMatch [re|^#(?!!)|]
        , saIsCommentEnd = isMatch [re|^#(?!!)|]
        }
