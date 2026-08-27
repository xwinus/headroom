{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.CLISpec
    ( spec
    )
where

import Headroom.Meta
    ( globalConfigDirName
    , globalConfigFileName
    )
import RIO
import RIO.Directory (createDirectoryIfMissing)
import RIO.FilePath ((</>))
import System.Environment (getEnvironment)
import System.Process
    ( CreateProcess (..)
    , proc
    , readCreateProcessWithExitCode
    )
import Test.Hspec

spec :: Spec
spec = do
    describe "headroom CLI" $ do
        it "prints a structured error for invalid project configuration"
            . withHeadroomCli globalConfig invalidProjectConfig
            $ assertStructuredError "project" "Unknown run mode: typo"

withHeadroomCli
    :: Text
    -> Text
    -> ((ExitCode, String) -> IO a)
    -> IO a
withHeadroomCli global project action =
    withSystemTempDirectory "headroom-cli" $ \directory -> do
        let homeDirectory = directory </> "home"
            projectDirectory = directory </> "project"
            globalDirectory = homeDirectory </> globalConfigDirName
            globalPath = globalDirectory </> globalConfigFileName
            projectPath = projectDirectory </> ".headroom.yaml"
        createDirectoryIfMissing True globalDirectory
        createDirectoryIfMissing True projectDirectory
        writeFileUtf8 globalPath global
        writeFileUtf8 projectPath project
        environment <- isolatedHome homeDirectory <$> getEnvironment
        let command =
                (proc "headroom" ["run", "-c"])
                    { cwd = Just projectDirectory
                    , env = Just environment
                    }
        (exitCode, _, stderrOutput) <- readCreateProcessWithExitCode command ""
        action (exitCode, stderrOutput)

assertStructuredError :: String -> String -> (ExitCode, String) -> Expectation
assertStructuredError scope reason (exitCode, stderrOutput) = do
    exitCode `shouldBe` ExitFailure 1
    stderrOutput `shouldContain` "ERROR: Cannot parse " <> scope <> " configuration:"
    stderrOutput `shouldContain` reason
    stderrOutput `shouldNotContain` "CallStack"

isolatedHome :: FilePath -> [(String, String)] -> [(String, String)]
isolatedHome homeDirectory environment =
    ("HOME", homeDirectory)
        : filter ((/= "HOME") . fst) environment

globalConfig :: Text
globalConfig =
    "updates:\n"
        <> "  check-for-updates: false\n"
        <> "  update-interval-days: 7\n"

invalidProjectConfig :: Text
invalidProjectConfig =
    "version: 0.4.0.0\n"
        <> "run-mode: typo\n"
