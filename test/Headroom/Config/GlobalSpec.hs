{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.Config.GlobalSpec
    ( spec
    )
where

import Headroom.Config.Global
import Headroom.Config.Parse
    ( ConfigurationParseError (..)
    , ConfigurationScope (..)
    )
import Headroom.Data.Has (Has (..))
import Headroom.Data.Lens
    ( suffixLenses
    , suffixLensesFor
    )
import Headroom.Embedded (defaultGlobalConfig)
import Headroom.IO.FileSystem
    ( FileSystem (..)
    , mkFileSystem
    )
import Headroom.Meta
    ( globalConfigDirName
    , globalConfigFileName
    )
import RIO
import RIO.Directory (doesFileExist)
import RIO.FilePath ((</>))
import qualified RIO.Text as T
import Test.Hspec

data TestEnv = TestEnv
    { envFileSystem :: FileSystem (RIO TestEnv)
    }

suffixLenses ''TestEnv
suffixLensesFor
    [ "fsCreateDirectory"
    , "fsDoesFileExist"
    , "fsGetUserDirectory"
    , "fsWriteFile"
    ]
    ''FileSystem

instance Has (FileSystem (RIO TestEnv)) TestEnv where
    hasLens = envFileSystemL

env :: TestEnv
env =
    TestEnv
        { envFileSystem =
            FileSystem
                { fsCreateDirectory = undefined
                , fsDoesDirectoryExist = undefined
                , fsDoesFileExist = undefined
                , fsFindFiles = undefined
                , fsFindFilesByExts = undefined
                , fsFindFilesByTypes = undefined
                , fsGetCurrentDirectory = undefined
                , fsGetPermissions = undefined
                , fsGetUserDirectory = undefined
                , fsListFiles = undefined
                , fsLoadFile = undefined
                , fsRemoveDirectory = undefined
                , fsRemoveFile = undefined
                , fsRenameFile = undefined
                , fsSetPermissions = undefined
                , fsWriteTempFile = undefined
                , fsWriteFile = undefined
                }
        }

spec :: Spec
spec = do
    describe "initGlobalConfigIfNeeded" $ do
        it "creates new config file and parent directory when needed" $ do
            withSystemTempDirectory "global-config" $ \dir -> do
                let cfgPath = dir </> globalConfigDirName </> globalConfigFileName
                    fsCreateDirectory0 = fsCreateDirectory mkFileSystem
                    fsWriteFile0 = fsWriteFile mkFileSystem
                    env' =
                        env
                            & (envFileSystemL . fsCreateDirectoryL .~ fsCreateDirectory0)
                            & (envFileSystemL . fsDoesFileExistL .~ fsDoesFileExist')
                            & (envFileSystemL . fsGetUserDirectoryL .~ fsGetUserDirectory')
                            & (envFileSystemL . fsWriteFileL .~ fsWriteFile0)
                    fsDoesFileExist' = const . pure $ False
                    fsGetUserDirectory' = pure dir
                _ <- runRIO env' initGlobalConfigIfNeeded
                result <- doesFileExist cfgPath
                result `shouldBe` True

    describe "parseGlobalConfig" $ do
        it "parses embedded default config YAML" $ do
            parseGlobalConfig defaultGlobalConfig `shouldSatisfy` isRight

        it "returns a structured error for an invalid configuration value" $ do
            let yaml = "updates:\n  check-for-updates: invalid\n  update-interval-days: 7"
            (parseGlobalConfig yaml :: IO GlobalConfig)
                `shouldThrow` configurationErrorContaining "expected Bool"

        it "returns a structured error for malformed YAML" $ do
            (parseGlobalConfig "updates: [" :: IO GlobalConfig)
                `shouldThrow` configurationErrorContaining "YAML parse exception"

configurationErrorContaining :: Text -> ConfigurationParseError -> Bool
configurationErrorContaining expected (ConfigurationParseError scope parseError) =
    scope
        == GlobalConfiguration
        && expected
        `T.isInfixOf` T.pack (displayException parseError)
