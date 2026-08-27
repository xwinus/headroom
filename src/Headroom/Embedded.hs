{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Embedded
-- Description : Embedded files
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Contains contents of files embedded using the "Data.FileEmbed" module.
module Headroom.Embedded
    ( configFileStub
    , defaultConfig
    , defaultGlobalConfig
    , licenseTemplate
    )
where

import Headroom.Config.Types (LicenseType (..))
import Headroom.Embedded.TH
    ( embedConfigFile
    , embedDefaultConfig
    , embedDefaultGlobalConfig
    , embedTemplate
    )
import Headroom.FileType.Types (FileType (..))
import RIO

-- | Content of dummy /YAML/ configuration file for the application.
configFileStub :: (IsString a) => a
configFileStub = $(embedConfigFile)

-- | Default /YAML/ configuration.
defaultConfig :: (IsString a) => a
defaultConfig = $(embedDefaultConfig)

-- | Default /YAML/ configuration for the global configuration file.
defaultGlobalConfig :: (IsString a) => a
defaultGlobalConfig = $(embedDefaultGlobalConfig)

-- | License template for given license.
licenseTemplate
    :: (IsString a)
    => LicenseType
    -- ^ license for which to return the template
    -> FileType
    -- ^ license for which to return the template
    -> a
    -- ^ template text
licenseTemplate licenseType fileType = case licenseType of
    Apache2 -> case fileType of
        C -> $(embedTemplate Apache2 C)
        CPP -> $(embedTemplate Apache2 CPP)
        CSS -> $(embedTemplate Apache2 CSS)
        Dart -> $(embedTemplate Apache2 Dart)
        Go -> $(embedTemplate Apache2 Go)
        Haskell -> $(embedTemplate Apache2 Haskell)
        HTML -> $(embedTemplate Apache2 HTML)
        Java -> $(embedTemplate Apache2 Java)
        JS -> $(embedTemplate Apache2 JS)
        Kotlin -> $(embedTemplate Apache2 Kotlin)
        PHP -> $(embedTemplate Apache2 PHP)
        PureScript -> $(embedTemplate Apache2 PureScript)
        Python -> $(embedTemplate Apache2 Python)
        Rust -> $(embedTemplate Apache2 Rust)
        Scala -> $(embedTemplate Apache2 Scala)
        Shell -> $(embedTemplate Apache2 Shell)
        XML -> $(embedTemplate Apache2 XML)
    BSD3 -> case fileType of
        C -> $(embedTemplate BSD3 C)
        CPP -> $(embedTemplate BSD3 CPP)
        CSS -> $(embedTemplate BSD3 CSS)
        Dart -> $(embedTemplate BSD3 Dart)
        Go -> $(embedTemplate BSD3 Go)
        Haskell -> $(embedTemplate BSD3 Haskell)
        HTML -> $(embedTemplate BSD3 HTML)
        Java -> $(embedTemplate BSD3 Java)
        JS -> $(embedTemplate BSD3 JS)
        Kotlin -> $(embedTemplate BSD3 Kotlin)
        PHP -> $(embedTemplate BSD3 PHP)
        PureScript -> $(embedTemplate BSD3 PureScript)
        Python -> $(embedTemplate BSD3 Python)
        Rust -> $(embedTemplate BSD3 Rust)
        Scala -> $(embedTemplate BSD3 Scala)
        Shell -> $(embedTemplate BSD3 Shell)
        XML -> $(embedTemplate BSD3 XML)
    GPL2 -> case fileType of
        C -> $(embedTemplate GPL2 C)
        CPP -> $(embedTemplate GPL2 CPP)
        CSS -> $(embedTemplate GPL2 CSS)
        Dart -> $(embedTemplate GPL2 Dart)
        Go -> $(embedTemplate GPL2 Go)
        Haskell -> $(embedTemplate GPL2 Haskell)
        HTML -> $(embedTemplate GPL2 HTML)
        Java -> $(embedTemplate GPL2 Java)
        JS -> $(embedTemplate GPL2 JS)
        Kotlin -> $(embedTemplate GPL2 Kotlin)
        PHP -> $(embedTemplate GPL2 PHP)
        PureScript -> $(embedTemplate GPL2 PureScript)
        Python -> $(embedTemplate GPL2 Python)
        Rust -> $(embedTemplate GPL2 Rust)
        Scala -> $(embedTemplate GPL2 Scala)
        Shell -> $(embedTemplate GPL2 Shell)
        XML -> $(embedTemplate GPL2 XML)
    GPL3 -> case fileType of
        C -> $(embedTemplate GPL3 C)
        CPP -> $(embedTemplate GPL3 CPP)
        CSS -> $(embedTemplate GPL3 CSS)
        Dart -> $(embedTemplate GPL3 Dart)
        Go -> $(embedTemplate GPL3 Go)
        Haskell -> $(embedTemplate GPL3 Haskell)
        HTML -> $(embedTemplate GPL3 HTML)
        Java -> $(embedTemplate GPL3 Java)
        JS -> $(embedTemplate GPL3 JS)
        Kotlin -> $(embedTemplate GPL3 Kotlin)
        PHP -> $(embedTemplate GPL3 PHP)
        PureScript -> $(embedTemplate GPL3 PureScript)
        Python -> $(embedTemplate GPL3 Python)
        Rust -> $(embedTemplate GPL3 Rust)
        Scala -> $(embedTemplate GPL3 Scala)
        Shell -> $(embedTemplate GPL3 Shell)
        XML -> $(embedTemplate GPL3 XML)
    MIT -> case fileType of
        C -> $(embedTemplate MIT C)
        CPP -> $(embedTemplate MIT CPP)
        CSS -> $(embedTemplate MIT CSS)
        Dart -> $(embedTemplate MIT Dart)
        Go -> $(embedTemplate MIT Go)
        Haskell -> $(embedTemplate MIT Haskell)
        HTML -> $(embedTemplate MIT HTML)
        Java -> $(embedTemplate MIT Java)
        JS -> $(embedTemplate MIT JS)
        Kotlin -> $(embedTemplate MIT Kotlin)
        PHP -> $(embedTemplate MIT PHP)
        PureScript -> $(embedTemplate MIT PureScript)
        Python -> $(embedTemplate MIT Python)
        Rust -> $(embedTemplate MIT Rust)
        Scala -> $(embedTemplate MIT Scala)
        Shell -> $(embedTemplate MIT Shell)
        XML -> $(embedTemplate MIT XML)
    MPL2 -> case fileType of
        C -> $(embedTemplate MPL2 C)
        CPP -> $(embedTemplate MPL2 CPP)
        CSS -> $(embedTemplate MPL2 CSS)
        Dart -> $(embedTemplate MPL2 Dart)
        Go -> $(embedTemplate MPL2 Go)
        Haskell -> $(embedTemplate MPL2 Haskell)
        HTML -> $(embedTemplate MPL2 HTML)
        Java -> $(embedTemplate MPL2 Java)
        JS -> $(embedTemplate MPL2 JS)
        Kotlin -> $(embedTemplate MPL2 Kotlin)
        PHP -> $(embedTemplate MPL2 PHP)
        PureScript -> $(embedTemplate MPL2 PureScript)
        Python -> $(embedTemplate MPL2 Python)
        Rust -> $(embedTemplate MPL2 Rust)
        Scala -> $(embedTemplate MPL2 Scala)
        Shell -> $(embedTemplate MPL2 Shell)
        XML -> $(embedTemplate MPL2 XML)
