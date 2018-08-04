module Untyped.CoreSpec where

import Untyped.Syntax
import Untyped.Core

import Test.Hspec

spec :: Spec
spec = do
  describe "small-step evaluation" $ do
         it "'(\\. 1 0 2) (\\. 0)' should be '0 (\\. 0) 1'" $ do
                eval p88_gamma p88_ex01 `shouldBe` NTmApp (NTmApp (NTmVar 0 0) (NTmAbs "y" (NTmVar 0 1))) (NTmVar 1 0)

p88_gamma = []
p88_ex01 = NTmApp (NTmAbs "x" (NTmApp (NTmApp (NTmVar 1 1) (NTmVar 0 1)) (NTmVar 2 1))) (NTmAbs "y" (NTmVar 0 1))
