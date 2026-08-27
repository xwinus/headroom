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
-- Copyright   : (c) 2019-2023 Vaclav Svejcar
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

import Control.Monad.Logger (runNoLoggingT)
import Data.String.Interpolate (iii)
import Database.Persist
    ( PersistStoreRead (..)
    , PersistStoreWrite (..)
    )
import Database.Persist.Sqlite
    ( ConnectionPool
    , SqlBackend
    , SqliteConnectionInfo
    , createSqlitePoolFromInfo
    , extraPragmas
    , mkSqliteConnectionInfo
    , runMigrationSilent
    , runSqlPool
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
-- The underlying database is opened (and migrated) lazily, upon the first access
-- to the store, so commands that never touch the store don't pay for it.
sqliteKVStore
    :: (MonadIO m)
    => StorePath
    -- ^ path of the store location
    -> m (KVStore m)
    -- ^ store instance
sqliteKVStore sp = do
    poolVar <- newMVar Nothing
    pure
        KVStore
            { kvGetValue = getValueSQLite poolVar sp
            , kvPutValue = putValueSQLite poolVar sp
            }

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

getValueSQLite
    :: (MonadIO m) => MVar (Maybe ConnectionPool) -> StorePath -> GetValueFn m
getValueSQLite poolVar sp (ValueKey key) =
    withStore poolVar sp $ do
        maybeValue <- get $ StoreRecordKey key
        pure $ case maybeValue of
            Just (StoreRecord v) -> decodeValue v
            Nothing -> Nothing

putValueSQLite
    :: (MonadIO m) => MVar (Maybe ConnectionPool) -> StorePath -> PutValueFn m
putValueSQLite poolVar sp (ValueKey key) value =
    withStore poolVar sp
        $ repsert (StoreRecordKey key) (StoreRecord $ encodeValue value)

-- | Number of milliseconds that /SQLite/ itself waits for a lock held by another
-- process before giving up with @SQLITE_BUSY@.
busyTimeout :: Int
busyTimeout = 5000

-- | Delays (in microseconds) between individual attempts to re-run a transaction
-- that failed because the store was locked by another process.
retryDelays :: [Int]
retryDelays = take 8 $ iterate (* 2) 25000

-- | Connection info for the store, tuned for concurrent access from multiple
-- /Headroom/ processes sharing the same store file.
connectionInfo :: Text -> SqliteConnectionInfo
connectionInfo path =
    mkSqliteConnectionInfo path
        & extraPragmas
        .~ ["PRAGMA busy_timeout = " <> tshow busyTimeout <> ";"]

-- | Returns the pool of connections to the store, opening the underlying database
-- and running the migration upon the first call.
storePool :: MVar (Maybe ConnectionPool) -> Text -> IO ConnectionPool
storePool poolVar path = modifyMVar poolVar $ \case
    Just pool -> pure (Just pool, pool)
    Nothing -> do
        pool <- runNoLoggingT $ createSqlitePoolFromInfo (connectionInfo path) 1
        withRetry path . runSqlPool (void $ runMigrationSilent migrateAll) $ pool
        pure (Just pool, pool)

-- | Runs given action against the store.
withStore
    :: (MonadIO m)
    => MVar (Maybe ConnectionPool)
    -> StorePath
    -> ReaderT SqlBackend IO a
    -> m a
withStore poolVar (StorePath path) action = liftIO $ do
    pool <- storePool poolVar path
    withRetry path $ runSqlPool action pool

-- | Runs given action, retrying with exponential backoff if it fails because the
-- store is locked by another process, and wrapping any other failure into
-- 'KVStoreError'. Note that the @busy_timeout@ pragma alone isn't enough here, as
-- /SQLite/ doesn't apply it to @SQLITE_BUSY_SNAPSHOT@, raised when a read
-- transaction is being upgraded to a write one.
withRetry :: Text -> IO a -> IO a
withRetry path action = go retryDelays `catchAny` wrapError
  where
    go delays =
        try action >>= \case
            Right result -> pure result
            Left ex
                | isLocked ex
                , (delay : rest) <- delays ->
                    threadDelay delay >> go rest
                | otherwise -> throwIO ex
    isLocked ex = seError ex `elem` [ErrorBusy, ErrorLocked]
    wrapError = throwIO . CannotAccessStore path . T.pack . displayException
