{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Test.Server where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad.App          (runAppM)
import           Data.Config
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Storage.Client             (StorageEnv (..), getObject,
                                             postObject, withClient)
import           Storage.Environment
import           Storage.Logger             (logInfo, withLogger)
import           Storage.Metrics            (newMetrics)
import           Storage.Metrics.Carbon     (Carbon (..))
import           Storage.Persist            (PersistStore (..))
import           Storage.Server             (app)

import           Data.Merkle.Hash

import           Pipes.ByteString           (fromLazy, toLazyM)

import           Network.Wai.Handler.Warp   (testWithApplication)

import           Control.Monad.Trans.Except (ExceptT (..))
import           Servant                    (Handler (..))
import           Servant.Client             (parseBaseUrl)

import           Control.Lens               (set)
import           Data.Generics.Product      (field)


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

lookupIx :: Int -> [a] -> Maybe a
lookupIx _n [] = Nothing
lookupIx n xs  = let ix = n `mod` length xs in Just (xs !! n)

spec :: Spec
spec =
  around withServer $ describe "Server" $
    describe "Dummy test" $
      it "Just starts the server" $ \port ->
        True `shouldBe` True
