module Untyped.CoreSpec where

import Untyped.Syntax
import Untyped.Core

import Test.Hspec

spec :: Spec
spec = do
  describe "small-step evaluation" $ do
         it "'(\\. 1 0 2) (\\. 0)' should be '0 (\\. 0) 1'" $ do
                eval p88_gamma p88_ex01 `shouldBe` TmApp (TmApp (TmVar 0 0) (TmAbs "y" (TmVar 0 1))) (TmVar 1 0)

p88_gamma = []
p88_ex01 = TmApp (TmAbs "x" (TmApp (TmApp (TmVar 1 1) (TmVar 0 1)) (TmVar 2 1))) (TmAbs "y" (TmVar 0 1))
