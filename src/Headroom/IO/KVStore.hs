{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Headroom.IO.KVStore
-- Description : Key-value persistent store
-- Copyright   : (c) 2019-2026 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is really simple /key-value/ persistent store that uses /SQLite/ as a
-- backend. Main goal is to provide /type-safe/ way how to define value keys, that
-- can be later used to set/put the actual value into the store.
module Headroom.IO.KVStore
    ( -- * Type Aliases
      GetValueFn
    , PutValueFn
    , KVStore (..)

      -- *  Type Classes
    , ValueCodec (..)

      -- * Data Types
    , ValueKey (..)
    , StorePath (..)

      -- * Error Data Types
    , KVStoreError (..)

      -- * Public Functions
    , inMemoryKVStore
    , sqliteKVStore
    , valueKey
    )
where

import Data.String.Interpolate (iii)
import Database.Persist
    ( PersistStoreRead (..)
    , PersistStoreWrite (..)
    )
import Database.Persist.Sqlite
    ( SqliteConnectionInfo
    , extraPragmas
    , mkSqliteConnectionInfo
    , runMigrationSilent
    , runSqliteInfo
    , walEnabled
    )
import Database.Persist.TH
    ( mkMigrate
    , mkPersist
    , persistLowerCase
    , share
    , sqlSettings
    )
import Database.Sqlite
    ( Error (..)
    , SqliteException (..)
    )
import GHC.Clock (getMonotonicTimeNSec)
import Headroom.Types
    ( fromHeadroomError
    , toHeadroomError
    )
import RIO
import RIO.List (iterate)
import qualified RIO.Map as M
import qualified RIO.Text as T
import RIO.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    , parseTimeM
    )

------------------------------  TEMPLATE HASKELL  ------------------------------

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
StoreRecord
  Id Text
  value Text
  deriving Show
|]

--------------------------------  TYPE ALIASES  --------------------------------

-- | Gets the value for given 'ValueKey' from the store.
type GetValueFn m =
    forall a
     . (ValueCodec a)
    => ValueKey a
    -- ^ key for the value
    -> m (Maybe a)
    -- ^ value (if found)

-- | Puts the value for given 'ValueKey' into the store.
type PutValueFn m =
    forall a
     . (ValueCodec a)
    => ValueKey a
    -- ^ key for the value
    -> a
    -- ^ value to put into store
    -> m ()
    -- ^ operation result

-----------------------------  POLYMORPHIC RECORD  -----------------------------

-- | /Polymorphic record/ composed of /key-value/ store operations, allowing to
-- abstract over concrete implementation without (ab)using /type classes/.
data KVStore m = KVStore
    { kvGetValue :: GetValueFn m
    , kvPutValue :: PutValueFn m
    }

-- | Constructs persistent instance of 'KVStore' that uses /SQLite/ as a backend.
sqliteKVStore
    :: (MonadIO m)
    => StorePath
    -- ^ path of the store location
    -> KVStore m
    -- ^ store instance
sqliteKVStore sp =
    KVStore{kvGetValue = getValueSQLite sp, kvPutValue = putValueSQLite sp}

-- | Constructs non-persistent in-memory instance of 'KVStore'.
inMemoryKVStore :: (MonadIO m) => m (KVStore m)
inMemoryKVStore = do
    ref <- newIORef M.empty
    pure
        KVStore
            { kvGetValue = getValueInMemory ref
            , kvPutValue = putValueInMemory ref
            }

--------------------------------  TYPE CLASSES  --------------------------------

-- | Represents way how to encode/decode concrete types into textual
-- representation used by the store to hold values.
class ValueCodec a where
    -- | Encodes value into textual representation.
    encodeValue
        :: a
        -- ^ value to encode
        -> Text
        -- ^ textual representation

    -- | Decodes value from textual representation.
    decodeValue
        :: Text
        -- ^ value to decode
        -> Maybe a
        -- ^ decoded value (if available)

instance ValueCodec Text where
    encodeValue = id
    decodeValue = Just

instance ValueCodec UTCTime where
    encodeValue = T.pack . formatTime defaultTimeLocale "%FT%T%Q"
    decodeValue = parseTimeM True defaultTimeLocale "%FT%T%Q" . T.unpack

---------------------------------  DATA TYPES  ---------------------------------

-- | /Type-safe/ representation of the key for specific value.
newtype ValueKey a = ValueKey Text deriving (Eq, Show)

-- | Constructor function for 'ValueKey'.
valueKey :: Text -> ValueKey a
valueKey = ValueKey

-- | Path to the store (e.g. path of the /SQLite/ database on filesystem).
newtype StorePath = StorePath Text deriving (Eq, Show)

---------------------------------  ERROR TYPES  --------------------------------

-- | Error during accessing the /key-value/ store.
data KVStoreError
    = -- | store cannot be opened or accessed
      CannotAccessStore Text Text
    deriving (Eq, Show, Typeable)

instance Exception KVStoreError where
    displayException = displayException'
    toException = toHeadroomError
    fromException = fromHeadroomError

displayException' :: KVStoreError -> String
displayException' = \case
    CannotAccessStore path reason ->
        [iii|
    Cannot access the key-value store at #{path}, reason: #{reason}.
  |]

------------------------------  PRIVATE FUNCTIONS  -----------------------------

getValueInMemory :: (MonadIO m) => IORef (Map Text Text) -> GetValueFn m
getValueInMemory ref (ValueKey key) = do
    storeMap <- readIORef ref
    pure $ M.lookup key storeMap >>= decodeValue

putValueInMemory :: (MonadIO m) => IORef (Map Text Text) -> PutValueFn m
putValueInMemory ref (ValueKey key) value = do
    modifyIORef ref $ M.insert key (encodeValue value)
    pure ()

getValueSQLite :: (MonadIO m) => StorePath -> GetValueFn m
getValueSQLite (StorePath path) (ValueKey key) =
    liftIO . withRetry path . runSqliteInfo (connectionInfo path) $ do
        void $ runMigrationSilent migrateAll
        maybeValue <- get $ StoreRecordKey key
        pure $ case maybeValue of
            Just (StoreRecord v) -> decodeValue v
            Nothing -> Nothing

putValueSQLite :: (MonadIO m) => StorePath -> PutValueFn m
putValueSQLite (StorePath path) (ValueKey key) value =
    liftIO . withRetry path . runSqliteInfo (connectionInfo path) $ do
        void $ runMigrationSilent migrateAll
        repsert (StoreRecordKey key) (StoreRecord $ encodeValue value)

-- | Number of milliseconds that an individual SQLite operation waits for a lock.
busyTimeout :: Int
busyTimeout = 100

-- | Maximum time in microseconds spent opening and accessing the store, including
-- all retries. The store is only used by the optional update check, so it must
-- fail quickly instead of delaying the main command.
retryBudget :: Int
retryBudget = 2000000

-- | Delays (in microseconds) between individual attempts to re-run a transaction
-- that failed because the store was locked by another process.
retryDelays :: [Int]
retryDelays = iterate (min 200000 . (* 2)) 10000

-- | Connection info for the store, tuned for concurrent access from multiple
-- /Headroom/ processes sharing the same store file.
connectionInfo :: Text -> SqliteConnectionInfo
connectionInfo path =
    infoWithoutDefaultWAL
        & extraPragmas
        .~ [ "PRAGMA busy_timeout = " <> tshow busyTimeout <> ";"
           , "PRAGMA journal_mode = WAL;"
           ]
  where
    infoWithoutDefaultWAL = mkSqliteConnectionInfo path & walEnabled .~ False

-- | Runs given action within a single time budget, retrying with jittered
-- exponential backoff if the store is locked by another process. The whole action
-- is retried because SQLite can raise @SQLITE_BUSY_SNAPSHOT@ when a read
-- transaction is upgraded to a write transaction.
withRetry :: Text -> IO a -> IO a
withRetry path action = do
    outcome <- tryAny . timeout retryBudget $ go retryDelays
    case outcome of
        Left ex -> wrapError ex
        Right Nothing ->
            throwIO
                . CannotAccessStore path
                $ "access timed out after "
                <> tshow (retryBudget `div` 1000)
                <> " ms"
        Right (Just result) -> pure result
  where
    go (delay : rest) =
        try action >>= \case
            Right result -> pure result
            Left ex
                | isLocked ex -> do
                    actualDelay <- jitter delay
                    threadDelay actualDelay
                    go rest
                | otherwise -> throwIO ex
    go [] = action
    isLocked ex = seError ex `elem` [ErrorBusy, ErrorLocked]
    wrapError = throwIO . CannotAccessStore path . T.pack . displayException

-- | Adds a small amount of jitter so multiple processes that collided don't keep
-- retrying in lockstep.
jitter :: Int -> IO Int
jitter delay = do
    now <- getMonotonicTimeNSec
    let percentage = 75 + fromIntegral (now `mod` 51)
    pure $ delay * percentage `div` 100
