{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hatrix.Monad.Clock
     ( MonadClock (..)
     ) where

import qualified ClassyPrelude
import           Hatrix.Prelude


class Monad m ⇒ MonadClock m where
  getCurrentTime ∷ m UTCTime


instance (Monad m, MonadIO m) ⇒ MonadClock m where
  getCurrentTime = liftIO ClassyPrelude.getCurrentTime
