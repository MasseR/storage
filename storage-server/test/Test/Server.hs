{-# LANGUAGE RecordWildCards #-}
module Test.Server where

import           Test.Hspec

import           Storage.Environment
import           Storage.Logger      (logInfo, withLogger)

-- withServer :: (Int -> IO a) -> IO a
-- withServer f = withTestEnv $ \env ->
--   _

-- withTestEnv :: (Env -> IO a) -> IO a
-- withTestEnv f = withLogger $ \logginEnv -> do
--   let config = Config { port = 0
--                       , persistStore = _
--                       , carbon = 
--   metrics <- newMetrics
--   let environment = Env { .. }
--   f environment

spec :: Spec
spec = return ()
