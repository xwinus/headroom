{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.FileType.Types
-- Description : Data types for "Headroom.FileType"
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains data types for "Headroom.FileType" modules.
module Headroom.FileType.Types where

import Headroom.Data.EnumExtra (EnumExtra (..))
import RIO

-- | Supported type of source code file.
data FileType
    = -- | support for /C/ programming language
      C
    | -- | support for /C++/ programming language
      CPP
    | -- | support for /CSS/
      CSS
    | -- | support for /Dart/ programming language
      Dart
    | -- | support for /Go/ programming language
      Go
    | -- | support for /Haskell/ programming language
      Haskell
    | -- | support for /HTML/
      HTML
    | -- | support for /Java/ programming language
      Java
    | -- | support for /JavaScript/ programming language
      JS
    | -- | support for /Kotlin/ programming language
      Kotlin
    | -- | support for /Nix/ programming language
      Nix
    | -- | support for /PHP/ programming language
      PHP
    | -- | support for /PureScript/ programming language
      PureScript
    | -- | support for /Python/ programming language
      Python
    | -- | support for /Rust/ programming language
      Rust
    | -- | support for /Scala/ programming language
      Scala
    | -- | support for /Shell/
      Shell
    | -- | support for /XML/
      XML
    deriving (Bounded, Enum, EnumExtra, Eq, Ord, Show)
