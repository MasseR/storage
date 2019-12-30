module Storage.Persist
  ( PersistStore(..)
  , HasPersistStore(..)
  , persistStore
  , writeObject
  , writeTree
  )
  where

import           Control.Comonad             (extract, extend)
import           Control.Lens                (Iso', Lens', iso, lens, view)
import qualified Data.Binary                 as Binary
import           Data.ByteString.Strict.Lens (unpackedChars)
import           Data.Merkle                 (Merkle)
import           Data.Merkle.Hash            (Hash, base16)
import           MyPrelude
import           System.FilePath.Posix

newtype PersistStore = PersistStore { persistRoot :: FilePath }
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

writeObject :: (MonadIO m, MonadReader r m, HasPersistStore r) => Hash -> ByteString -> m ()
writeObject h content = do
  ObjectStore path <- view (persistStore . objectStore)
  let file = view unpackedChars $ base16 h
  writeFile (path </> file) content

writeTree :: (MonadIO m, MonadReader r m, HasPersistStore r) => Merkle Hash -> m ()
writeTree m = do
  TreeStore path <- view (persistStore . treeStore)
  sequence_ $ extend (go path) m
  where
    go path tree = do
      let file = view unpackedChars . base16 . extract $ tree
      liftIO $ Binary.encodeFile (path </> file) tree
