{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hatrix.GUI.Component
     ( Box (..)
     , UIElement (..)
     , EventSubscription (..)
     , Event (..)
     , Component (..)
     , ComponentMeta (..)
     ) where

import           Hatrix.Prelude

import           Data.Default


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


data ComponentMeta
   = ComponentMeta
   { label         ∷ Maybe Text
   , borderWidth   ∷ Maybe ℤ
   , isModal       ∷ 𝔹
   , quitOnDestroy ∷ 𝔹
   } deriving (Show, Eq)

instance Default ComponentMeta where
  def
    = ComponentMeta
    { label         = Nothing
    , borderWidth   = Nothing
    , isModal       = False
    , quitOnDestroy = False
    }


data Monad m
   ⇒ Component id s m
   = Component ComponentMeta s (s → Maybe id → Event → m s) (Box id)
