module Fail.RuntimeCheckGo where

open import Haskell.Prelude
open import Haskell.Extra.Dec

conflict : (((go0 : Nat) → @0 IsTrue (go0 > 0) → Nat) → Nat) → Nat
conflict _ = 0
{-# COMPILE AGDA2HS conflict #-}
