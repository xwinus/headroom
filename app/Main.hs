{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Main
-- Description : Main application launcher
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Code responsible for booting up the application and parsing command line
-- arguments.
module Main where

import Headroom.Command (commandParser)
import Headroom.Command.Gen
    ( commandGen
    , parseGenMode
    )
import Headroom.Command.Init (commandInit)
import Headroom.Command.Run (commandRun)
import Headroom.Command.Types
    ( Command (..)
    , CommandGenOptions (..)
    , CommandInitOptions (..)
    , CommandRunOptions (..)
    )
import Headroom.Types (HeadroomError (..))
import Options.Applicative (execParser)
import RIO
import System.IO (hPutStrLn)

main :: IO ()
main = do
    command' <- execParser commandParser
    catch
        (bootstrap command')
        ( \ex -> do
            hPutStrLn stderr $ "ERROR: " <> displayException (ex :: HeadroomError)
            exitWith $ ExitFailure 1
        )

bootstrap :: Command -> IO ()
bootstrap = \case
    c@(Gen _ _) -> do
        cgoGenMode <- parseGenMode c
        commandGen CommandGenOptions{..}
    Init cioLicenseType cioSourcePaths -> commandInit CommandInitOptions{..}
    Run croSourcePaths croExcludedPaths croExcludeIgnoredPaths croBuiltInTemplates croTemplateRefs croVariables croRunMode croDebug croDryRun ->
        commandRun CommandRunOptions{..}
