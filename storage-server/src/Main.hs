module Main where

import MyPrelude

import Control.Monad.App
import Storage.Environment (Env(..))

import Storage.Logger (logInfo, withLogger)

main :: IO ()
main = withLogger $ \l -> runAppM (Env l) $
  logInfo "Hello"
