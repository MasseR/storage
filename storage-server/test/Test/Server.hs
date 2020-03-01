{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Test.Server where

import           Prelude                     hiding (assert)

import           Data.GenValidity
import           Data.GenValidity.ByteString ()

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Control.Monad.App           (runAppM)
import           Control.Monad.State         (MonadState, execStateT)
import           Control.Monad.Trans         (lift)
import           Control.Monad.Trans.Maybe   (MaybeT (..), runMaybeT)

import           Data.Config
import           Network.HTTP.Client         (defaultManagerSettings,
                                              newManager)
import           Storage.Client              (StorageEnv (..), getObject,
                                              postObject, withClient)
import           Storage.Environment
import           Storage.Logger              (logInfo, withLogger)
import           Storage.Metrics             (newMetrics)
import           Storage.Metrics.Carbon      (Carbon (..))
import           Storage.Persist             (PersistStore (..))
import           Storage.Server              (app)

import           Data.Merkle.Hash

import           Pipes.ByteString            (fromLazy, toLazyM)

import           Network.Wai.Handler.Warp    (testWithApplication)

import           Control.Monad.Trans.Except  (ExceptT (..))
import           Servant                     (Handler (..))
import           Servant.Client              (parseBaseUrl)

import           Control.Lens                (set, (<>=))
import           Data.Generics.Product       (field)


withServer :: (StorageEnv -> IO a) -> IO a
withServer f = withTestEnv $ \env ->
  testWithApplication (pure (app (nat env))) $ \port -> do
    manager <- newManager defaultManagerSettings
    base <- set (field @"baseUrlPort") port <$> parseBaseUrl "http://localhost"
    let storage = StorageEnv{..}
    f storage
  where
    nat env = Servant.Handler . ExceptT . try . runAppM env

withTestEnv :: (Env -> IO a) -> IO a
withTestEnv f =
  withLogger $ \loggingEnv ->
    withSystemTempDirectory "storage-test" $ \path -> do
      let config = Config { port = 0
                          , persistStore = PersistStore path
                          , carbon = Carbon Nothing
                          }
      metrics <- newMetrics
      let environment = Env { .. }
      f environment

writeFileStorage :: LByteString -> ReaderT StorageEnv IO (Maybe Hash)
writeFileStorage lbs =
  withClient (postObject (fromLazy lbs)) (pure . either (const Nothing) Just)

readFileStorage :: Hash -> ReaderT StorageEnv IO (Maybe LByteString)
readFileStorage h =
  withClient (getObject h) $ \case
    Left e -> pure Nothing
    Right producer -> Just <$> liftIO (toLazyM producer)

newtype Model
  = Model { hashes :: [ Hash ] }
  deriving (Generic)

emptyModel :: Model
emptyModel = Model { hashes = [] }

lookupIx :: Int -> [a] -> Maybe a
lookupIx _n [] = Nothing
lookupIx n xs  = let ix = n `mod` length xs in Just (xs !! n)

execWrite :: ( MonadIO m, MonadState Model m ) => StorageEnv -> LByteString -> PropertyM m ()
execWrite storage lbs = void $ runMaybeT $ do
  h <- MaybeT . run . liftIO $ runReaderT (writeFileStorage lbs) storage
  got <- MaybeT . run . liftIO $ runReaderT (readFileStorage h) storage
  lift (assert (got == lbs))

  lift (run (field @"hashes" <>= [h]))

newtype Command = WriteObject LByteString
  deriving Show

instance Arbitrary Command where
  arbitrary = WriteObject <$> genValid
  shrink (WriteObject lbs) = WriteObject <$> shrinkValid lbs

prop_ops :: StorageEnv -> [Command] -> Property
prop_ops storage commands =
  monadic exec $ for_ commands $ \case
    WriteObject lbs -> execWrite storage lbs
  where
    shower = \case
      WriteObject _ -> "WriteObject"
    hashLabeler s | length (hashes s) > 30 = "> 30 hashes"
                  | length (hashes s) > 20 = "> 20 hashes"
                  | length (hashes s) > 10 = "> 10 hashes"
                  | otherwise              = "< 10 hashes"
    exec f = ioProperty $ do
      s <- execStateT f emptyModel
      let labels = label (hashLabeler s) . tabulate "Command" (map shower commands)
      pure (labels ())

spec :: Spec
spec =
  around withServer $ describe "Server" $
      it "Can read and write objects" $ \storage ->
        forAll arbitrary (prop_ops storage)
