module Storage where

import           MyPrelude

import           Data.Merkle

import           Storage.Persist

import qualified Data.List.NonEmpty as NE

import           Pipes
import qualified Pipes.ByteString   as PB
import qualified Pipes.Prelude      as P

build
  :: (MonadIO m, Monad m, MonadReader r m, HasPersistStore r)
  => Producer ByteString m ()
  -> m (Maybe (Merkle Hash))
build p = do
  tree <- fmap combine . NE.nonEmpty <$> P.toListM hashes
  for_ tree writeTree
  pure tree
  where
    hashes =
      PB.chunksOf' chunk p
        >-> P.map (\x -> (x, hash x))
        >-> P.mapM (\(x, h) -> Leaf h <$ writeObject h x)
    chunk :: Int
    chunk = 2 ^ (19 :: Int) -- 512k

consume
  :: (MonadUnliftIO m, Monad m, MonadReader r m, HasPersistStore r)
  => Hash
  -> m (Maybe (Producer ByteString m ()))
consume h = do
  tree <- readTree h
  traverse (pure . consume' . leaves) tree
  where
    consume' xs = each xs >-> P.mapM readObject
