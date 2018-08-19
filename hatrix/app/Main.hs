{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Hatrix.Prelude
import           Hatrix.Config
import           Hatrix.AppContext
import           Hatrix.Logger
import           Hatrix.Monad.MVar
import           Hatrix.Monad.Thread
import           Hatrix.Monad.LoggerBus

import           Data.Default
import           Text.InterpolatedString.QM

import           Control.Monad.Logger (runStdoutLoggingT)


main ∷ IO ()
main = do
  loggerBus' ← newEmptyMVar

  let appContext = AppContext
        { loggerBus  = loggerBus'
        }

  flip runReaderT appContext $ do

    -- Running logger thread
    _ <- fork $ runStdoutLoggingT writeLoggerBusEventsToMonadLogger

    !(config ∷ Config) ←
      liftIO readConfig >>= \case
        Right config → pure config
        Left  errMsg → def <$
          logError [qms| Parsing config failed with message: {errMsg},
                         using default config… |]

    logInfo [qm| Foo {config}… |]
    logError [qm| Bar… |]
