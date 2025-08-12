module Fail.RuntimeCheckTransparent where

open import Haskell.Prelude
open import Haskell.Extra.Dec

transparent : Nat → Nat
transparent n = n
{-# COMPILE AGDA2HS transparent transparent #-}

noTransparent : (x : Nat) → ⦃@0 _ : IsTrue (x > 0)⦄ → Nat
noTransparent n = n
{-# COMPILE AGDA2HS noTransparent transparent #-}
