{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hatrix.GUI.Component
     ( Box (..)
     , UIElement (..)
     , EventSubscription (..)
     , Event (..)
     , Component (..)
     ) where

import           Hatrix.Prelude


data Box id
   = Vertical   [UIElement id]
   | Horizontal [UIElement id]
     deriving (Show, Eq)


data UIElement id
   = Label { label ∷ Text }
   | TextInput { identifier ∷ Maybe id, subscriptions ∷ [EventSubscription] }
   | Button { identifier ∷ Maybe id, subscriptions ∷ [EventSubscription], label ∷ Text }
     deriving (Show, Eq)


data EventSubscription
   = ValueChange
   | MouseClick
   | FocusStateChange
     deriving (Show, Eq)


data Event
   = ValueIsChanged Text
   | Clicked
   | Focus 𝔹
     deriving (Show, Eq)


data Monad m ⇒ Component id s m = Component s (s → Maybe id → Event → m s) (Box id)
