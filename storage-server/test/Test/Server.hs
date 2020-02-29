{-# LANGUAGE RecordWildCards #-}
module Test.Server where

import           Test.Hspec

import           Control.Monad.App          (runAppM)
import           Data.Config
import           Storage.Environment
import           Storage.Logger             (logInfo, withLogger)
import           Storage.Metrics            (newMetrics)
import           Storage.Metrics.Carbon     (Carbon (..))
import           Storage.Persist            (PersistStore (..))
import           Storage.Server             (app)

import           Network.Wai.Handler.Warp   (testWithApplication)

import           Control.Monad.Trans.Except (ExceptT (..))
import           Servant                    (Handler (..))


withServer :: (Int -> IO a) -> IO a
withServer f = withTestEnv $ \env ->
  testWithApplication (pure (app (nat env))) f
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

spec :: Spec
spec = around withServer $ describe "Server" $
  describe "Dummy test" $
    it "Just starts the server" $ \port ->
      True `shouldBe` True
