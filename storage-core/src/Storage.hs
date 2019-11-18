module Storage where

import           MyPrelude

import           Data.Merkle

import qualified Data.List.NonEmpty as NE

import           Pipes
import qualified Pipes.ByteString   as PB
import qualified Pipes.Prelude      as P

build :: MonadIO m => Handle -> m (Maybe (Merkle Hash))
build h = fmap combine . NE.nonEmpty <$> runEffect (P.toListM hashes)
  where
    hashes =
      PB.hGetSome 4096 h
        >-> P.map (\x -> (x, hash x))
        >-> P.mapM pure -- Write the chunks to persisten storage
        >-> P.map (Leaf . snd)
