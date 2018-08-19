{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Hatrix.Monad.MVar
     ( MonadMVar (..)
     ) where

import qualified ClassyPrelude
import           Hatrix.Prelude


class Monad m ⇒ MonadMVar m where
  newEmptyMVar ∷ m (MVar a)
  newMVar      ∷ a → m (MVar a)
  putMVar      ∷ MVar a → a → m ()
  takeMVar     ∷ MVar a → m a


instance (Monad m, MonadIO m) ⇒ MonadMVar m where
  newEmptyMVar = ClassyPrelude.newEmptyMVar
  newMVar      = ClassyPrelude.newMVar
  putMVar      = ClassyPrelude.putMVar
  takeMVar     = ClassyPrelude.takeMVar
