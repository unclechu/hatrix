{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Hatrix.Prelude
     ( module ClassyPrelude
     , module Prelude.Unicode
     , module Data.Function
     , module Data.Functor
     , (•)
     , (◇)
     , (×)
     , (□), (◲), (■)
     , type 𝔹
     ) where

import           ClassyPrelude hiding (newEmptyMVar, newMVar, putMVar, takeMVar, getCurrentTime)
import           Prelude.Unicode
import           Data.Function ((&))
import           Data.Functor ((<&>))


type 𝔹 = Bool


-- Left-to-right composition, just like (>=>) for monads.
(•) ∷ (α → β) → (β → γ) → (α → γ)
(•) = flip (∘)
{-# INLINE (•) #-}
infixl 9 •


-- Generic concatenation
(◇) ∷ Monoid α ⇒ α → α → α
(◇) = (<>)
{-# INLINE (◇) #-}
infixr 6 ◇


-- Better multiplication operator (better alternative to `(⋅)`)
(×) ∷ Num α ⇒ α → α → α
(×) = (*)
{-# INLINE (×) #-}
infixl 7 ×


-- Unicode exponent operators

(□) ∷ (Num α, Integral β) ⇒ α → β → α
(□) = (^)
{-# INLINE (□) #-}
infixr 8 □

(◲) ∷ (Fractional α, Integral β) ⇒ α → β → α
(◲) = (^^)
{-# INLINE (◲) #-}
infixr 8 ◲

(■) ∷ Floating α ⇒ α → α → α
(■) = (**)
{-# INLINE (■) #-}
infixr 8 ■
