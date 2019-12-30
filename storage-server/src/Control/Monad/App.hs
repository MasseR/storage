{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Control.Monad.App
  (
    AppM
  , runAppM
  )
  where

import           Katip               (Katip, KatipContext)
import           MyPrelude
import           Storage.Logger      (LogM (..))

import           Storage.Environment (Env)

newtype AppM a = AppM (ReaderT Env IO a)

deriving newtype instance Functor AppM
deriving newtype instance Applicative AppM
deriving newtype instance Monad AppM
deriving newtype instance MonadIO AppM
deriving newtype instance MonadReader Env AppM
deriving newtype instance MonadUnliftIO AppM
deriving via (LogM Env) instance Katip AppM
deriving via (LogM Env) instance KatipContext AppM

runAppM :: Env -> AppM a -> IO a
runAppM e (AppM r) = runReaderT r e
