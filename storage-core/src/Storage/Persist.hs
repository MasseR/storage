{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Storage.Persist
  ( PersistStore(..)
  , HasPersistStore(..)
  , persistStore
  , writeObject
  , writeTree
  , readTree
  , readObject
  )
  where

import           Control.Comonad            (extend, extract)
import           Control.Lens               (Iso', Lens', iso, lens, re, view)
import           Data.Merkle                (Merkle)
import           Data.Merkle.Hash           (Hash, hashed, _Text)
import           Data.Text.Strict.Lens      (unpacked)
import           MyPrelude
import           System.FilePath.Posix
import           UnliftIO.Directory

import           System.IO.Error            (isDoesNotExistError)

import           Data.SafeCopy              (SafeCopy, safeGet, safePut)
import           Data.Serialize             (runGet, runPut)

import           Data.Aeson                 (FromJSON, ToJSON)

import           Data.GenValidity
import           Data.GenValidity.Arbitrary
import           Test.QuickCheck            (Arbitrary (..),
                                             PrintableString (..))

newtype PersistStore = PersistStore { persistRoot :: FilePath }
  deriving (Show, ToJSON, FromJSON, Validity, Eq)
  deriving Arbitrary via (ViaGenValid PersistStore)

instance GenValid PersistStore where
  genValid = PersistStore . getPrintableString <$> arbitrary
  shrinkValid (PersistStore st) = PersistStore . getPrintableString <$> shrink (PrintableString st)

newtype ObjectStore = ObjectStore { objectRoot :: FilePath }
newtype TreeStore = TreeStore { treeRoot :: FilePath }

class HasPersistStore a where
  getPersistStore :: a -> PersistStore
  setPersistStore :: a -> PersistStore -> a

persistStore :: HasPersistStore a => Lens' a PersistStore
persistStore = lens getPersistStore setPersistStore

objectStore :: Iso' PersistStore ObjectStore
objectStore = iso toObj fromObj
  where
    toObj = ObjectStore . (</> "objects") . persistRoot
    fromObj = PersistStore . takeDirectory . objectRoot

treeStore :: Iso' PersistStore TreeStore
treeStore = iso toTree fromTree
  where
    toTree = TreeStore . (</> "trees") . persistRoot
    fromTree = PersistStore . takeDirectory . treeRoot

-- | Create a file with a prefix
hashPath :: Hash -> FilePath
hashPath h = prefix </> suffix
  where
    base16 :: Hash -> String
    base16 = view (re hashed . re _Text . unpacked)
    full = base16 h
    (prefix,suffix) = splitAt 3 full

writeObject :: (MonadIO m, MonadReader r m, HasPersistStore r) => Hash -> ByteString -> m ()
writeObject h content = do
  ObjectStore path <- view (persistStore . objectStore)
  let file = path </> hashPath h
  createDirectoryIfMissing True (takeDirectory file)
  writeFile file content

readObject :: (MonadIO m, MonadReader r m, HasPersistStore r) => Hash -> m ByteString
readObject h = do
  ObjectStore path <- view (persistStore . objectStore)
  let file = path </> hashPath h
  readFile file

writeTree :: (MonadIO m, MonadReader r m, HasPersistStore r) => Merkle Hash -> m ()
writeTree m = do
  TreeStore path <- view (persistStore . treeStore)
  sequence_ $ extend (go path) m
  where
    go path tree = do
      let file = path </> (hashPath . extract $ tree)
      createDirectoryIfMissing True (takeDirectory file)
      encodeFile file tree

encodeFile :: (MonadIO m, SafeCopy a) => FilePath -> a -> m ()
encodeFile path = writeFile path . runPut . safePut

decodeFile :: (MonadIO m, SafeCopy a) => FilePath -> m (Either String a)
decodeFile path = runGet safeGet <$> readFile path

readTree
  :: (MonadUnliftIO m, MonadIO m, MonadReader r m, HasPersistStore r)
  => Hash
  -> m (Maybe (Merkle Hash))
readTree h = do
  TreeStore path <- view (persistStore . treeStore)
  let file = path </> hashPath h
  catch
    (either (const Nothing) Just <$> decodeFile file)
    (\e -> if isDoesNotExistError e then pure Nothing else throwIO e)
