{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Command
-- Description : Support for parsing command line arguments
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains code responsible for parsing command line arguments, using
-- the /optparse-applicative/ library.
module Headroom.Command
    ( commandParser
    )
where

import Headroom.Command.Readers
    ( licenseReader
    , licenseTypeReader
    , regexReader
    , templateRefReader
    )
import Headroom.Command.Types (Command (..))
import Headroom.Config.Types
    ( LicenseType
    , RunMode (..)
    )
import Headroom.Data.EnumExtra (EnumExtra (..))
import Headroom.Meta
    ( buildVersion
    , productDesc
    , productInfo
    )
import Headroom.Meta.Version (printVersion)
import Options.Applicative
import RIO
import qualified RIO.Text as T

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Parses command line arguments.
commandParser :: ParserInfo Command
commandParser =
    info
        (helper <*> versionP <*> commandP)
        (fullDesc <> progDesc (T.unpack productDesc) <> header (T.unpack productInfo))

------------------------------  PRIVATE FUNCTIONS  -----------------------------

versionP :: Parser (a -> a)
versionP = versionInfoP <*> versionNumP
  where
    versionInfoP =
        infoOption
            (T.unpack productInfo)
            (long "version" <> help "show version info")
    versionNumP =
        infoOption
            (T.unpack . printVersion $ buildVersion)
            (long "numeric-version" <> help "show only version number")

commandP :: Parser Command
commandP = subparser (runCommand <> genCommand <> initCommand)
  where
    runCommand =
        command
            "run"
            ( info
                (runOptions <**> helper)
                (progDesc "add, replace, drop or check source code headers")
            )
    genCommand =
        command
            "gen"
            ( info
                (genOptions <**> helper)
                (progDesc "generate stub configuration and template files")
            )
    initCommand =
        command
            "init"
            ( info
                (initOptions <**> helper)
                (progDesc "initialize current project for Headroom")
            )

runOptions :: Parser Command
runOptions =
    Run
        <$> sourcePathsP
        <*> many
            ( option
                regexReader
                ( long "excluded-path"
                    <> short 'e'
                    <> metavar "REGEX"
                    <> help
                        "path to exclude from source code file paths"
                )
            )
        <*> switch
            ( long "exclude-ignored-paths"
                <> help "exclude paths ignored by used VCS"
            )
        <*> optional
            ( option
                licenseTypeReader
                ( long "builtin-templates"
                    <> metavar "licenseType"
                    <> help
                        "use built-in templates of selected license type"
                )
            )
        <*> many
            ( option
                templateRefReader
                ( long "template-path"
                    <> short 't'
                    <> metavar "PATH"
                    <> help
                        "path to template, can be either local file or directory or URL"
                )
            )
        <*> many
            ( strOption
                ( long "variable"
                    <> short 'v'
                    <> metavar "KEY=VALUE"
                    <> help
                        "value for template variable"
                )
            )
        <*> optional
            ( flag'
                Add
                ( long "add-headers"
                    <> short 'a'
                    <> help
                        "only adds missing license headers"
                )
                <|> flag'
                    Check
                    ( long "check-headers"
                        <> short 'c'
                        <> help
                            "check whether existing headers are up-to-date"
                    )
                <|> flag'
                    Replace
                    ( long "replace-headers"
                        <> short 'r'
                        <> help
                            "force replace existing license headers"
                    )
                <|> flag'
                    Drop
                    ( long "drop-headers"
                        <> short 'd'
                        <> help
                            "drop existing license headers only"
                    )
            )
        <*> switch (long "debug" <> help "produce more verbose output")
        <*> switch (long "dry-run" <> help "execute dry run (no changes to files)")

-- | Parses paths to source code files/directories, which can be given either
-- as (possibly repeated) @-s|--source-path@ options, or as positional
-- arguments. Paths from both sources are combined together.
sourcePathsP :: Parser [FilePath]
sourcePathsP = (<>) <$> optionsP <*> argumentsP
  where
    optionsP =
        many
            ( strOption
                ( long "source-path"
                    <> short 's'
                    <> metavar "PATH"
                    <> help
                        "path to source code file/directory"
                )
            )
    argumentsP =
        many
            ( strArgument
                ( metavar "PATHS..."
                    <> help
                        "path to source code file/directory"
                )
            )

genOptions :: Parser Command
genOptions =
    Gen
        <$> switch
            ( long "config-file"
                <> short 'c'
                <> help
                    "generate stub YAML config file to stdout"
            )
        <*> optional
            ( option
                licenseReader
                ( long "license"
                    <> short 'l'
                    <> metavar "licenseType:fileType"
                    <> help "generate template for license and file type"
                )
            )

initOptions :: Parser Command
initOptions =
    Init
        <$> option
            licenseTypeReader
            ( long "license-type"
                <> short 'l'
                <> metavar "TYPE"
                <> help
                    ( "type of open source license, available options: "
                        <> T.unpack (T.toLower (allValuesToText @LicenseType))
                    )
            )
        <*> some
            ( strOption
                ( long "source-path"
                    <> short 's'
                    <> metavar "PATH"
                    <> help
                        "path to source code file/directory"
                )
            )
