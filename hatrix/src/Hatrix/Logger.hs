{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hatrix.Logger
     ( writeLoggerBusEventsToMonadLogger
     ) where

import           Hatrix.Prelude
import           Hatrix.Monad.LoggerBus
import           Hatrix.Monad.LoggerBus.Types

import           Control.Monad (forever)
import           Control.Monad.Logger (MonadLogger, logInfoN, logErrorN)


-- | Redirects events from logger bus to MonadLogger.
--   Supposed to be run in own thread (it's infinite).
writeLoggerBusEventsToMonadLogger ∷ (MonadLogger m, MonadLoggerBus m) ⇒ m ()
writeLoggerBusEventsToMonadLogger = forever $ do
  LogMessage msgType msg ← readLog

  case msgType of
       LogInfo  → logInfoN  msg
       LogError → logErrorN msg

