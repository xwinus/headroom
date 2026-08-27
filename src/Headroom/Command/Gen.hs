{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Command.Gen
-- Description : Handler for the @gen@ command.
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The @gen@ command is responsible for generating various files requied by
-- /Headroom/, such as /YAML/ configuration stubs or /Mustache/ license templates.
-- Run /Headroom/ using the @headroom gen help@ to see available options.
module Headroom.Command.Gen
    ( commandGen
    , parseGenMode
    )
where

import Data.String.Interpolate (iii)
import Headroom.Command.Types
    ( Command (..)
    , CommandGenOptions (..)
    )
import Headroom.Command.Utils (bootstrap)
import Headroom.Config.Enrich
    ( Enrich (..)
    , replaceEmptyValue
    , withText
    )
import Headroom.Config.Types (GenMode (..))
import Headroom.Data.Lens (suffixLensesFor)
import Headroom.Embedded
    ( configFileStub
    , licenseTemplate
    )
import Headroom.Meta (buildVersion)
import Headroom.Meta.Version (printVersion)
import Headroom.Types
    ( fromHeadroomError
    , toHeadroomError
    )
import RIO
import qualified RIO.Text as T
import Prelude (putStrLn)

---------------------------------  DATA TYPES  ---------------------------------

data Env = Env
    { envLogFunc :: !LogFunc
    -- ^ logging function
    , envGenOptions :: !CommandGenOptions
    -- ^ options
    }

suffixLensesFor ["envLogFunc"] ''Env

instance HasLogFunc Env where
    logFuncL = envLogFuncL

env' :: CommandGenOptions -> LogFunc -> IO Env
env' opts logFunc = pure $ Env{envLogFunc = logFunc, envGenOptions = opts}

------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Parses 'GenMode' from combination of options from given 'Command'.
parseGenMode
    :: (MonadThrow m)
    => Command
    -- ^ command from which to parse the 'GenMode'
    -> m GenMode
    -- ^ parsed 'GenMode'
parseGenMode = \case
    Gen True Nothing -> pure GenConfigFile
    Gen False (Just license) -> pure $ GenLicense license
    _ -> throwM NoGenModeSelected

-- | Handler for /Generator/ command.
commandGen
    :: CommandGenOptions
    -- ^ /Generator/ command options
    -> IO ()
    -- ^ execution result
commandGen opts = bootstrap (env' opts) False $ case cgoGenMode opts of
    GenConfigFile -> liftIO printConfigFile
    GenLicense (lType, fType) -> liftIO . putStrLn $ licenseTemplate lType fType

------------------------------  PRIVATE FUNCTIONS  -----------------------------

printConfigFile :: IO ()
printConfigFile = putStrLn . T.unpack $ enrich modify configFileStub
  where
    modify = replaceEmptyValue "version" $ withText ver
    ver = printVersion buildVersion

---------------------------------  ERROR TYPES  --------------------------------

-- | Exception specific to the @gen@ command.
data CommandGenError
    = -- | no mode of /Gen/ command selected
      NoGenModeSelected
    deriving (Eq, Show)

instance Exception CommandGenError where
    displayException = displayException'
    toException = toHeadroomError
    fromException = fromHeadroomError

displayException' :: CommandGenError -> String
displayException' = \case
    NoGenModeSelected ->
        [iii|
      Please select at least one option what to generate
      (see --help for details)
    |]
