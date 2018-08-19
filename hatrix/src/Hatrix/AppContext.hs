{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hatrix.AppContext
     ( AppContext (..)
     ) where

import           Hatrix.Prelude
import           Hatrix.Monad.LoggerBus.Types (LogMessage)


data AppContext
   = AppContext
   { loggerBus ∷ MVar LogMessage
   }
