{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileSupport.C
-- Description : Support for /C/ source code files
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Basic support for /C/ source code files. This implementation doesn't extract any
-- variables or template data.
module Headroom.FileSupport.C
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
import Headroom.FileType.Types (FileType (C))

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Implementation of 'FileSupport' for /C/.
fileSupport :: FileSupport
fileSupport = defaultFileSupport C syntaxAnalysis

------------------------------  PRIVATE FUNCTIONS  -----------------------------

syntaxAnalysis :: SyntaxAnalysis
syntaxAnalysis =
    SyntaxAnalysis
        { saIsCommentStart = isMatch [re|^\/\*|^\/\/|]
        , saIsCommentEnd = isMatch [re|\*\/|^\/\/|]
        }
