{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Hatrix.Monad.Thread
     ( MonadThread (..)
     , forkWithWaitBus
     ) where

import           Hatrix.Prelude

import           Control.Exception (SomeException)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified UnliftIO.Concurrent as UnliftIO

import           Hatrix.Monad.MVar


class Monad m ⇒ MonadThread m where
  fork        ∷ m () → m UnliftIO.ThreadId
  forkFinally ∷ m a → (Either SomeException a → m ()) → m UnliftIO.ThreadId
  killThread  ∷ UnliftIO.ThreadId → m ()


instance (Monad m, MonadUnliftIO m, MonadIO m) ⇒ MonadThread m where
  fork        = UnliftIO.forkIO
  forkFinally = UnliftIO.forkFinally
  killThread  = UnliftIO.killThread


-- | Forks and returns @MVar@ which will be notified when thread is done.
forkWithWaitBus ∷ (MonadThread m, MonadMVar m) ⇒ m () → m (UnliftIO.ThreadId, MVar ())
forkWithWaitBus m = do
  waitBus <- newEmptyMVar
  (,waitBus) <$> forkFinally m (\_ → putMVar waitBus ())
