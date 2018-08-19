{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hatrix.Monad.LoggerBus
     ( MonadLoggerBus (..)
     ) where

import           Hatrix.Prelude
import           Hatrix.Monad.LoggerBus.Types
import           Hatrix.Monad.MVar
import           Hatrix.Monad.Clock
import           Hatrix.Monad.Thread
import           Hatrix.AppContext

import           Data.Time.Format (FormatTime)
import           Text.InterpolatedString.QM


class Monad m ⇒ MonadLoggerBus m where
  logInfo  ∷ Text → m ()
  logError ∷ Text → m ()
  readLog  ∷ m LogMessage


instance ( Monad m
         , MonadMVar m
         , MonadClock m
         , MonadThread m
         , MonadReader AppContext m
         ) ⇒ MonadLoggerBus m
         where

  logInfo msg = do
    loggerBus' ← asks loggerBus
    !utc ← getCurrentTime

    void $ fork $ -- Forking for non-blocking writing to @MVar@
      putMVar loggerBus' $ LogMessage LogInfo
        [qm| [{myTimeFormat utc} UTC] {msg} |]

  logError msg = do
    loggerBus' ← asks loggerBus
    !utc ← getCurrentTime

    void $ fork $ -- Forking for non-blocking writing to @MVar@
      putMVar loggerBus' $ LogMessage LogError
        [qm| [{myTimeFormat utc} UTC] {msg} |]

  readLog = asks loggerBus >>= takeMVar


myTimeFormat ∷ FormatTime t ⇒ t → String
myTimeFormat = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
