{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
    ( main
    )
where

import Criterion.Main
    ( bench
    , defaultMain
    , envWithCleanup
    , whnfIO
    )
import Headroom.IO.FileSystem
    ( WalkOptions (..)
    , walkFiles
    )
import RIO
import qualified RIO.Directory as D
import RIO.FilePath
    ( splitDirectories
    , (</>)
    )
import qualified RIO.List as L
import System.IO (openTempFile)

main :: IO ()
main =
    defaultMain
        [ envWithCleanup setupTree D.removeDirectoryRecursive $ \tree ->
            bench "large tree dominated by ignored files"
                . whnfIO
                $ walkFiles (walkOptions tree) (const $ pure ())
        ]

walkOptions :: FilePath -> WalkOptions IO
walkOptions root =
    WalkOptions
        { woSourcePaths = [root]
        , woIncludeFile = (".hs" `L.isSuffixOf`)
        , woExcludePath = const False
        , woIgnorePath = \_ -> pure . elem "ignored" . splitDirectories
        }

setupTree :: IO FilePath
setupTree = do
    temporaryDirectory <- D.getTemporaryDirectory
    (root, temporaryHandle) <-
        openTempFile temporaryDirectory "headroom-walker-benchmark"
    hClose temporaryHandle
    D.removeFile root
    D.createDirectory root
    let ignoredRoot = root </> "ignored"
        sourceRoot = root </> "src"
    D.createDirectory ignoredRoot
    D.createDirectory sourceRoot
    forM_ [1 .. 50 :: Int] $ \directoryNumber -> do
        let directory = ignoredRoot </> show directoryNumber
        D.createDirectory directory
        forM_ [1 .. 100 :: Int] $ \fileNumber ->
            writeFileUtf8
                (directory </> show fileNumber <> ".hs")
                "module Ignored where"
    writeFileUtf8 (sourceRoot </> "Main.hs") "module Main where"
    writeFileUtf8 (sourceRoot </> "Util.hs") "module Util where"
    pure root
