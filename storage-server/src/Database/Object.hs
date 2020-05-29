{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Database.Object where

import           MyPrelude

import           Data.Functor.Identity

import           Data.Merkle.Hash                 (Hash (..), _Text, hashed)

import           Database.Beam                    (Beamable, Columnar,
                                                   DataType (..),
                                                   FromBackendRow (..),
                                                   Table (..), timestamptz)
import           Database.Beam.Migrate
import           Database.Beam.Sqlite.Connection  (Sqlite)
import           Database.Beam.Sqlite.Syntax      (sqliteTextType)
import           Database.SQLite.Simple.FromField

import           Data.Proxy                       (Proxy (..))
import           Data.Time                        (LocalTime)

import           Control.Lens                     (makeLenses, preview)

newtype HashKey = HashKey Hash
  deriving (Show, Eq, Ord)

instance HasDefaultSqlDataType Sqlite HashKey where
  defaultSqlDataType _ _ _ = sqliteTextType

instance FromBackendRow Sqlite HashKey where

instance FromField HashKey where
  fromField = maybe (fail "Not a hash") (pure . HashKey) . preview (_Text . hashed) <=< fromField
    -- pure (_ x)

data ObjectT f
  = Object { _objectId :: Columnar f HashKey
           , _created  :: Columnar f LocalTime
           }

makeLenses ''ObjectT

type Object = ObjectT Identity

deriving instance Generic (ObjectT f)
deriving instance Show Object
deriving instance Eq Object
deriving instance Ord Object
deriving instance Beamable ObjectT

instance Table ObjectT where
  data PrimaryKey ObjectT f = ObjectId (Columnar f HashKey)
  primaryKey = ObjectId . _objectId

deriving instance Generic (PrimaryKey ObjectT f)
deriving instance Beamable (PrimaryKey ObjectT)
deriving instance Show (PrimaryKey ObjectT Identity)
deriving instance Eq (PrimaryKey ObjectT Identity)
deriving instance Ord (PrimaryKey ObjectT Identity)

migObjectT :: TableSchema Sqlite ObjectT
migObjectT =
  Object { _objectId = field "object_hash" (DataType (defaultSqlDataType (Proxy :: Proxy HashKey) (Proxy :: Proxy Sqlite) False))
         , _created = field "created" timestamptz
         }
