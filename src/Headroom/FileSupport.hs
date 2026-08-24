{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileSupport
-- Description : Support for handling various source code file types
-- Copyright   : (c) 2019-2023 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- /Headroom/ currently supports working with file types defined in 'FileType'
-- type, and because every type of source code file requires different handling of
-- some aspects, this file type specific support is implemented for every supported
-- file type and exposed as instance of 'FileSupport' data type.
module Headroom.FileSupport
    ( fileSupport
    , analyzeSourceCode
    )
where

import Control.Monad.State
    ( get
    , put
    )
import qualified Headroom.FileSupport.C as C
import qualified Headroom.FileSupport.CPP as CPP
import qualified Headroom.FileSupport.CSS as CSS
import qualified Headroom.FileSupport.Dart as Dart
import qualified Headroom.FileSupport.Go as Go
import qualified Headroom.FileSupport.HTML as HTML
import qualified Headroom.FileSupport.Haskell as Haskell
import qualified Headroom.FileSupport.JS as JS
import qualified Headroom.FileSupport.Java as Java
import qualified Headroom.FileSupport.Kotlin as Kotlin
import qualified Headroom.FileSupport.PHP as PHP
import qualified Headroom.FileSupport.PureScript as PureScript
import qualified Headroom.FileSupport.Python as Python
import qualified Headroom.FileSupport.Rust as Rust
import qualified Headroom.FileSupport.Scala as Scala
import qualified Headroom.FileSupport.Shell as Shell
import Headroom.FileSupport.Types
    ( FileSupport (..)
    , SyntaxAnalysis (..)
    )
import qualified Headroom.FileSupport.XML as XML
import Headroom.FileType.Types (FileType (..))
import Headroom.SourceCode
    ( LineType (..)
    , SourceCode
    , fromText
    )
import RIO
import qualified RIO.Text as T

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Returns 'FileSupport' for corresponding 'FileType'.
fileSupport :: FileType -> FileSupport
fileSupport C = C.fileSupport
fileSupport CPP = CPP.fileSupport
fileSupport CSS = CSS.fileSupport
fileSupport Dart = Dart.fileSupport
fileSupport Go = Go.fileSupport
fileSupport Haskell = Haskell.fileSupport
fileSupport HTML = HTML.fileSupport
fileSupport Java = Java.fileSupport
fileSupport JS = JS.fileSupport
fileSupport Kotlin = Kotlin.fileSupport
fileSupport PHP = PHP.fileSupport
fileSupport PureScript = PureScript.fileSupport
fileSupport Python = Python.fileSupport
fileSupport Rust = Rust.fileSupport
fileSupport Scala = Scala.fileSupport
fileSupport Shell = Shell.fileSupport
fileSupport XML = XML.fileSupport

-- | Analyzes the raw source code of given type using provided 'FileSupport'.
analyzeSourceCode
    :: FileSupport
    -- ^ 'FileSupport' implementation used for analysis
    -> Text
    -- ^ raw source code to analyze
    -> SourceCode
    -- ^ analyzed source code
analyzeSourceCode fs = fromText state0 process
  where
    SyntaxAnalysis{..} = fsSyntaxAnalysis fs
    state0 = 0 :: Int
    process (T.strip -> l) = do
        commentDepth <- get
        let isStart = saIsCommentStart
            isEnd = saIsCommentEnd
            (nextDepth, lineType) =
                if
                    | isStart l && isEnd l -> (commentDepth, Comment)
                    | isStart l -> (commentDepth + 1, Comment)
                    | commentDepth > 0 && isEnd l -> (commentDepth - 1, Comment)
                    | commentDepth > 0 -> (commentDepth, Comment)
                    | otherwise -> (0, Code)
        put nextDepth
        pure lineType
