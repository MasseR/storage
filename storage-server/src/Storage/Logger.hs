{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Storage.Logger
  (
  -- * Log initialization and setup
    LoggingEnv(..)
  , HasLoggingEnv(..)
  , WithLogging
  , LogM(..)
  , withLogger
  -- * Logging functions
  , logInfo
  , logError
  , logDebug
  , logWarning
  )
  where

import           Control.Lens         (Lens', lens, over, view)
import           Control.Monad.Reader (local)
import           GHC.Stack
import           Katip
import           MyPrelude

data LoggingEnv
  = LoggingEnv { logNamespace :: Namespace
               , logContexts  :: LogContexts
               , logEnv       :: LogEnv
               }

class HasLoggingEnv a where
  getLoggingEnv :: a -> LoggingEnv
  setLoggingEnv :: a -> LoggingEnv -> a

type WithLogging m = KatipContext m

loggingEnv :: HasLoggingEnv a => Lens' a LoggingEnv
loggingEnv = lens getLoggingEnv setLoggingEnv


namespace :: Lens' LoggingEnv Namespace
namespace = lens logNamespace (\e x -> e{logNamespace=x})

contexts :: Lens' LoggingEnv LogContexts
contexts = lens logContexts (\e x -> e{logContexts=x})

env :: Lens' LoggingEnv LogEnv
env = lens logEnv (\e x -> e{logEnv=x})

newtype LogM r a = LogM (ReaderT r IO a)

deriving newtype instance Functor (LogM r)
deriving newtype instance Applicative (LogM r)
deriving newtype instance Monad (LogM r)
deriving newtype instance MonadIO (LogM r)
deriving newtype instance MonadReader r (LogM r)

instance HasLoggingEnv r => Katip (LogM r) where
  getLogEnv = view (loggingEnv . env)
  localLogEnv f = local (over (loggingEnv . env) f)

instance HasLoggingEnv r => KatipContext (LogM r) where
  getKatipContext = view (loggingEnv . contexts)
  localKatipContext f = local (over (loggingEnv . contexts) f)
  getKatipNamespace = view (loggingEnv . namespace)
  localKatipNamespace f = local (over (loggingEnv . namespace) f)

withLogger :: (LoggingEnv -> IO a) -> IO a
withLogger m = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "storage" "production"
  bracket makeLogEnv closeScribes $ \le -> do
    let initialContext = mempty
        initialNamespace = "main"
    m (LoggingEnv initialNamespace initialContext le)


logInfo :: (HasCallStack, WithLogging m) => Text -> m ()
logInfo = withFrozenCallStack (logLocM InfoS . ls)

logError :: (HasCallStack, WithLogging m) => Text -> m ()
logError = withFrozenCallStack (logLocM ErrorS . ls)

logDebug :: (HasCallStack, WithLogging m) => Text -> m ()
logDebug = withFrozenCallStack (logLocM DebugS . ls)

logWarning :: (HasCallStack, WithLogging m) => Text -> m ()
logWarning = withFrozenCallStack (logLocM WarningS . ls)
