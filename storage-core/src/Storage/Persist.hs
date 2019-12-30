{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Storage.Persist
  ( PersistStore(..)
  , HasPersistStore(..)
  , persistStore
  , writeObject
  , writeTree
  )
  where

import           Control.Comonad             (extend, extract)
import           Control.Lens                (Iso', Lens', iso, lens, view)
import qualified Data.Binary                 as Binary
import           Data.ByteString.Strict.Lens (unpackedChars)
import           Data.Merkle                 (Merkle)
import           Data.Merkle.Hash            (Hash, base16)
import           MyPrelude
import           System.FilePath.Posix
import           UnliftIO.Directory

import           Data.Aeson

newtype PersistStore = PersistStore { persistRoot :: FilePath } deriving (Show, ToJSON, FromJSON)
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
    full = view unpackedChars $ base16 h
    (prefix,suffix) = splitAt 3 full

writeObject :: (MonadIO m, MonadReader r m, HasPersistStore r) => Hash -> ByteString -> m ()
writeObject h content = do
  ObjectStore path <- view (persistStore . objectStore)
  let file = path </> hashPath h
  createDirectoryIfMissing True (takeDirectory file)
  writeFile file content

writeTree :: (MonadIO m, MonadReader r m, HasPersistStore r) => Merkle Hash -> m ()
writeTree m = do
  TreeStore path <- view (persistStore . treeStore)
  sequence_ $ extend (go path) m
  where
    go path tree = do
      let file = path </> (hashPath . extract $ tree)
      createDirectoryIfMissing True (takeDirectory file)
      liftIO $ Binary.encodeFile file tree
