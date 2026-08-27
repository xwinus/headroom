{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.UI.Message
-- Description : UI component for message box
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Module providing UI component for message box (info/warning/error).
module Headroom.UI.Message
    ( MessageType (..)
    , Message (..)
    , messageInfo
    , messageWarn
    , messageError
    )
where

import Data.String.Interpolate (i)
import RIO

-- | Type of the message box (info/warning/error).
data MessageType
    = -- | info message type
      Info
    | -- | warning message type
      Warn
    | -- | error message type
      Error
    deriving (Eq, Show)

instance Display MessageType where
    textDisplay Info = "[i]"
    textDisplay Warn = "[!]"
    textDisplay Error = "[x]"

-- | Data type for message box.
data Message = Message MessageType Text
    deriving (Eq, Show)

instance Display Message where
    textDisplay (Message tp tx) = [i|#{textDisplay tp} #{tx}|]

-- | Creates 'Message' of type 'Info'.
messageInfo :: Text -> Message
messageInfo = Message Info

-- | Creates 'Message' of type 'Warn'.
messageWarn :: Text -> Message
messageWarn = Message Warn

-- | Creates 'Message' of type 'Error'.
messageError :: Text -> Message
messageError = Message Error
