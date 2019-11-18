{-# LANGUAGE NoImplicitPrelude #-}
module MyPrelude

  ( module X
  , module MyPrelude
  )

  where

import           Control.Monad.Trans  as X (MonadIO, liftIO)
import           Data.Text            as X (Text)
import           GHC.Generics         as X (Generic)
import           Prelude              as X hiding (putStrLn, readFile,
                                            writeFile)
import           System.IO            as X (Handle)

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Foldable        as F
import qualified Data.Text.IO         as T

type ByteString = B.ByteString
type LByteString = LB.ByteString

catMaybes :: Traversable t => t (Maybe a) -> [a]
catMaybes = concatMap F.toList

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . T.putStrLn
