{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Command.Utils
-- Description : Shared code for individual command handlers
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Contains shared code common to all command handlers.
module Headroom.Command.Utils
    ( bootstrap
    )
where

import RIO

-- | Bootstraps /RIO/ application using provided environment data and flag
-- whether to run in debug mode.
bootstrap
    :: (LogFunc -> IO env)
    -- ^ function returning environment data
    -> Bool
    -- ^ whether to run in debug mode
    -> RIO env a
    -- ^ /RIO/ application to execute
    -> IO a
    -- ^ execution result
bootstrap enfFn isDebug logic = do
    defLogOptions <- logOptionsHandle stderr isDebug
    withLogFunc (setLogUseLoc False defLogOptions) $ \logFunc -> do
        env <- liftIO $ enfFn logFunc
        runRIO env logic
