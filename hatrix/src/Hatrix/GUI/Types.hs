{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hatrix.GUI.Types
     ( Input (..)
     , Event (..)
     ) where

import           Hatrix.Prelude


data Input
   = TextInput { identifier ∷ Text, subscriptions ∷ [Event] }
   | Button { identifier ∷ Text, subscriptions ∷ [Event] }
     deriving (Show, Eq)


data Event
   = ValueIsChanged
   | Clicked
   | Focus 𝔹
     deriving (Show, Eq)
