{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Database
  ( module Database
  , Connection
  , Sqlite
  )
  where

import           MyPrelude

import           Control.Lens                    (Lens', lens, makeLenses, set,
                                                  view)

import           Database.Beam                   (Database, DatabaseSettings,
                                                  TableEntity)
import           Database.Beam.Migrate
import           Database.Beam.Sqlite.Connection (Sqlite, SqliteM,
                                                  runBeamSqlite)
import           Database.SQLite.Simple          (Connection)
import qualified Database.SQLite.Simple          as SQLite

-- Table definitions
import           Database.Object

class HasConnection a where
  {-# MINIMAL getConnection, setConnection | connection #-}
  getConnection :: a -> Connection
  getConnection = view connection

  setConnection :: a -> Connection -> a
  setConnection = flip (set connection)

  connection :: Lens' a Connection
  connection = lens getConnection setConnection

instance HasConnection Connection where
  getConnection = id
  setConnection _ b = b

type WithDB r m = (MonadReader r m, HasConnection r, MonadIO m)

runDB :: WithDB r m => SqliteM a -> m a
runDB f = view connection >>= \conn -> liftIO (runBeamSqlite conn f)

-- | Beam table definition
newtype Storage f
  = Storage { _objects :: f (TableEntity ObjectT) }

makeLenses ''Storage

deriving stock instance Generic (Storage f)
deriving anyclass instance (Database be) Storage

storage :: DatabaseSettings Sqlite Storage
storage = unCheckDatabase (evaluateDatabase migrationSteps)

migrationSteps :: MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite Storage)
migrationSteps = migrationStep "Initial schema" v1
  where
    -- Note, when actual *migrations* needs to be done, I need to read the docs
    -- on how to do it
    v1 :: () -> Migration Sqlite (CheckedDatabaseSettings Sqlite Storage)
    v1 _ = Storage <$> createTable "objects" migObjectT

withTransaction :: (WithDB r m, MonadUnliftIO m) => m a -> m a
withTransaction f = do
  conn <- view connection
  withRunInIO $ \runInIO -> SQLite.withTransaction conn (runInIO f)
