{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hatrix.Monad.LoggerBus.Types
     ( LogMessageType (..)
     , LogMessage (..)
     ) where

import           Hatrix.Prelude


data LogMessageType = LogInfo | LogError deriving (Show, Eq)
data LogMessage     = LogMessage LogMessageType Text deriving (Show, Eq)
