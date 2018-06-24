module Arith.CoreSpec where

import Arith.Core

import Control.Monad

import Arith.SyntaxSpec

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "small-step == big-step" $ do
         prop "(small-step t ->* v ==> big-step t ↓↓ v) && (big-step t ↓↓ v ==> small-step t ->* v)" prop_ex3_5_17

-- | exercise 3.5.17: t ->* v <=> t ↓↓ v
prop_ex3_5_17 t = prop_ex3_5_17_forward t .&&. prop_ex3_5_17_backward t

prop_ex3_5_17_forward t = let vs =  eval t
                          in isval vs ==> vs == evalBig t

prop_ex3_5_17_backward t = let vb =  evalBig t
                           in isval vb ==> eval t == vb
