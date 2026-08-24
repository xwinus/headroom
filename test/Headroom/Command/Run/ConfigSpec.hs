{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Command.Run.ConfigSpec
    ( spec
    )
where

import Headroom.Command.Run (loadTemplates)
import Headroom.Command.Run.Config (finalConfigurationFrom)
import Headroom.Command.Types (CommandRunOptions (..))
import Headroom.Config.Types
    ( AppConfig (..)
    , ConfigurationError (..)
    , ConfigurationKey (..)
    , CtAppConfig
    , LicenseType (..)
    )
import Headroom.Data.Has
    ( Has (..)
    )
import Headroom.Data.Lens (suffixLenses)
import Headroom.FileType.Types (FileType (..))
import Headroom.IO.FileSystem
    ( FileSystem
    , mkFileSystem
    )
import Headroom.IO.Network
    ( Network
    , mkNetwork
    )
import Headroom.Meta (buildVersion)
import Headroom.Meta.Version (printVersion)
import RIO
import RIO.FilePath ((</>))
import qualified RIO.Map as M
import Test.Hspec

data TestEnv = TestEnv
    { envLogFunc :: LogFunc
    , envRunOptions :: CommandRunOptions
    , envConfiguration :: CtAppConfig
    , envFileSystem :: FileSystem (RIO TestEnv)
    , envNetwork :: Network (RIO TestEnv)
    }

suffixLenses ''TestEnv

instance HasLogFunc TestEnv where
    logFuncL = envLogFuncL

instance Has CommandRunOptions TestEnv where
    hasLens = envRunOptionsL

instance Has CtAppConfig TestEnv where
    hasLens = envConfigurationL

instance Has (FileSystem (RIO TestEnv)) TestEnv where
    hasLens = envFileSystemL

instance Has (Network (RIO TestEnv)) TestEnv where
    hasLens = envNetworkL

spec :: Spec
spec = do
    describe "finalConfigurationFrom" $ do
        it "preserves built-in templates configured in YAML when the CLI option is absent"
            . withConfig "source-paths: [src]\nbuiltin-templates: mit\n"
            $ \path -> do
                config <- runRIO (testEnv Nothing []) $ finalConfigurationFrom path

                acBuiltInTemplates config `shouldBe` Just MIT

        it "allows the CLI option to override built-in templates configured in YAML"
            . withConfig "source-paths: [src]\nbuiltin-templates: mit\n"
            $ \path -> do
                config <- runRIO (testEnv (Just BSD3) []) $ finalConfigurationFrom path

                acBuiltInTemplates config `shouldBe` Just BSD3

        it "uses no built-in templates when neither YAML nor CLI configures them"
            . withConfig "source-paths: [src]\n"
            $ \path -> do
                config <- runRIO (testEnv Nothing []) $ finalConfigurationFrom path

                acBuiltInTemplates config `shouldBe` Nothing

        it "reports incomplete configuration after merging all layers"
            . withConfig ""
            $ \path -> do
                runRIO (testEnv Nothing []) (finalConfigurationFrom path)
                    `shouldThrow` (== MissingConfiguration CkSourcePaths)

        it "loads built-in templates selected only through YAML"
            . withConfig "source-paths: [src]\nbuiltin-templates: mit\n"
            $ \path -> do
                let env = testEnv Nothing []
                config <- runRIO env $ finalConfigurationFrom path
                templates <- runRIO env{envConfiguration = config} loadTemplates

                M.lookup Haskell templates `shouldSatisfy` isJust

withConfig :: ByteString -> (FilePath -> IO a) -> IO a
withConfig content action =
    withSystemTempDirectory "run-config" $ \directory -> do
        let path = directory </> ".headroom.yaml"
            version = encodeUtf8 $ printVersion buildVersion
        writeFileBinary path $ "version: \"" <> version <> "\"\n" <> content
        action path

testEnv :: Maybe LicenseType -> [FilePath] -> TestEnv
testEnv builtInTemplates sourcePaths = TestEnv{..}
  where
    envLogFunc = mkLogFunc $ \_ _ _ _ -> pure ()
    envRunOptions =
        CommandRunOptions
            { croRunMode = Nothing
            , croSourcePaths = sourcePaths
            , croExcludedPaths = []
            , croExcludeIgnoredPaths = False
            , croBuiltInTemplates = builtInTemplates
            , croTemplateRefs = []
            , croVariables = []
            , croDebug = False
            , croDryRun = False
            }
    envConfiguration = undefined
    envFileSystem = mkFileSystem
    envNetwork = mkNetwork
