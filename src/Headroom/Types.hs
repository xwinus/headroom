{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.Types
-- Description : Application data types
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Module containing most of the data types used by the application.
module Headroom.Types
    ( -- * Error Data Types
      HeadroomError (..)

      -- ** Helper Functions
    , fromHeadroomError
    , toHeadroomError

      -- * Other Data Types
    , CurrentYear (..)
    , LicenseType (..)
    )
where

import Data.Aeson
    ( FromJSON (..)
    , withText
    )
import Data.Typeable (cast)
import Headroom.Data.EnumExtra (EnumExtra (..))
import RIO
import qualified RIO.Text as T

-- | Top-level of the /Headroom/ exception hierarchy.
data HeadroomError = forall e. (Exception e) => HeadroomError e

instance Show HeadroomError where
    show (HeadroomError he) = show he

instance Exception HeadroomError where
    displayException (HeadroomError he) = displayException he

-- | Wraps given exception into 'HeadroomError'.
toHeadroomError
    :: (Exception e)
    => e
    -- ^  exception to wrap
    -> SomeException
    -- ^ wrapped exception
toHeadroomError = toException . HeadroomError

-- | Unwraps given exception from 'HeadroomError'.
fromHeadroomError
    :: (Exception e)
    => SomeException
    -- ^ exception to unwrap
    -> Maybe e
    -- ^ unwrapped exception
fromHeadroomError e = do
    HeadroomError he <- fromException e
    cast he

-- | Wraps the value of current year.
newtype CurrentYear = CurrentYear
    { unCurrentYear :: Integer
    -- ^ value of current year
    }
    deriving (Eq, Show)

-- | Supported type of open source license.
data LicenseType
    = -- | support for /Apache-2.0/ license
      Apache2
    | -- | support for /BSD-3-Clause/ license
      BSD3
    | -- | support for /GNU GPL2/ license
      GPL2
    | -- | support for /GNU GPL3/ license
      GPL3
    | -- | support for /MIT/ license
      MIT
    | -- | support for /MPL2/ license
      MPL2
    deriving (Bounded, Enum, EnumExtra, Eq, Ord, Show)

instance FromJSON LicenseType where
    parseJSON = withText "LicenseType" $ \raw ->
        case textToEnum raw of
            Just licenseType -> pure licenseType
            _ -> fail $ "Unknown license type: " <> T.unpack raw
