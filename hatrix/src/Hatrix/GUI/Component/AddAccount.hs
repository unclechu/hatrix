{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hatrix.GUI.Component.AddAccount
     ( addAccountComponent
     ) where

import           Hatrix.Prelude
import           Hatrix.GUI.Component
import           Hatrix.Monad.LoggerBus
import           Hatrix.AppContext

import           Data.Default
import           Text.InterpolatedString.QM


data State
   = State
   { authToken ∷ Text
   , homeServer ∷ Text
   } deriving (Show, Eq)

instance Default State where
  def
    = State
    { authToken = [qn| testing initial "authToken" |]
    , homeServer = ""
    }


data Identifier
   = AuthTokenId
   | HomeServerId
     deriving (Show, Eq)


type Constraint m = (Monad m, MonadReader AppContext m, MonadLoggerBus m)


addAccountComponent ∷ (Constraint m) ⇒ Component Identifier State m
addAccountComponent
  = Component meta def eventHandler
  $ Vertical
  [ Label { label = "Please enter account data:" }
  , TextInput { identifier = Just AuthTokenId
              , subscriptions = [ValueChange]
              }
  , TextInput { identifier = Just HomeServerId
              , subscriptions = [ValueChange]
              }
  ]
  where
    meta
      = def
      { label         = Just "Add an account"
      , borderWidth   = Just 10
      , quitOnDestroy = True
      }

    eventHandler ∷ (Constraint m) ⇒ State → Maybe Identifier → Event → m State
    eventHandler state id' event = do
      logInfo
        [qms| testing "AddAccount" component event:
              \  Identifier: {id'}
              \  Event: {event}
              |]
      pure state { homeServer = homeServer state ◇ "x" }
