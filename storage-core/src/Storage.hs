module Storage where

import           MyPrelude

import           Data.Merkle

import qualified Data.List.NonEmpty as NE

import           Pipes
import qualified Pipes.ByteString   as PB
import qualified Pipes.Prelude      as P

build :: Monad m => Producer ByteString m () -> m (Maybe (Merkle Hash))
build p = fmap combine . NE.nonEmpty <$> P.toListM hashes
  where
    hashes =
      PB.chunksOf' chunk p
        >-> P.map (\x -> (x, hash x))
        >-> P.mapM pure -- XXX: Write the chunks to disk
        >-> P.map (Leaf . snd)
    chunk :: Int
    chunk = 4096
