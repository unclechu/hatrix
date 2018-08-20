{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Hatrix.GUI.Implementation.GTK
     ( runGtkGUI
     ) where

import           Hatrix.Prelude hiding (on)
import           Hatrix.AppContext
import           Hatrix.GUI.Component
import           Hatrix.GUI.Component.AddAccount

import           Data.Maybe (fromMaybe)

import           Graphics.UI.Gtk


runGtkGUI ∷ (MonadIO m, MonadUnliftIO m, MonadReader AppContext m) ⇒ m ()
runGtkGUI = do
  _ ← liftIO initGUI
  renderComponent addAccountComponent
  liftIO mainGUI


renderComponent ∷ (MonadIO m, MonadUnliftIO m, MonadReader AppContext m) ⇒ Component id s m → m ()
renderComponent (Component ComponentMeta {..} initialState evHandler layout) = do
  wnd ← do
    wnd ← liftIO windowNew
    when quitOnDestroy $ liftIO $ void $ on wnd objectDestroy mainQuit
    let f = liftIO ∘ set wnd ∘ (:[])
    let opt = fromMaybe $ pure ()
    opt $ f ∘ (containerBorderWidth :=) ∘ fromInteger <$> borderWidth
    opt $ f ∘ (windowTitle :=) <$> label
    f $ windowModal := isModal
    pure wnd

  liftIO $ widgetShowAll wnd
