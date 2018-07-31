module Untyped.SyntaxSpec where

import Untyped.Syntax

import Test.Hspec

spec :: Spec
spec = do
  describe "show term" $ do
         it "'4 (3 2)' should be 'x (y z)'" $ do
                showTm p77_gamma p77_01 `shouldBe` "(x (y z))"

         it "'\\. 4 0' should be '\\w. y w'" $ do
                showTm p77_gamma p77_02 `shouldBe` "(\\w. (y w))"

         it "'\\.\\. 6' should be '\\w. \\a'. x'" $ do
                showTm p77_gamma p77_03 `shouldBe` "(\\w. (\\a'. x))"

  describe "shifting" $ do
         it "'↑ 2 (\\.\\. 1 (0 2))' should be '(\\.\\. 1 (0 4))'" $ do
                termShift 2 ex6_2_2_1 `shouldBe` TmAbs "x" (TmAbs "y" (TmApp (TmVar 1 4) (TmApp (TmVar 0 4) (TmVar 4 4))))

         it "'↑ 2 (\\. 0 1 (\\. 0 1 2))' should be '(\\. 0 3 (\\. 0 1 4))'" $ do
                termShift 2 ex6_2_2_2 `shouldBe` TmAbs "x" (TmApp (TmApp (TmVar 0 3) (TmVar 3 3)) (TmAbs "y" (TmApp (TmApp (TmVar 0 4) (TmVar 1 4)) (TmVar 4 4))))

--p77_gamma = zip ["x", "y", "z", "a", "b"] (repeat NameBind)
p77_gamma = zip ["b", "a", "z", "y", "x"] (repeat NameBind)
p77_01 = TmApp (TmVar 4 5) (TmApp (TmVar 3 5) (TmVar 2 5))
p77_02 = TmAbs "w" (TmApp (TmVar 4 6) (TmVar 0 6))
p77_03 = TmAbs "w" (TmAbs "a" (TmVar 6 7))

ex6_2_2_1 = TmAbs "x" (TmAbs "y" (TmApp (TmVar 1 2) (TmApp (TmVar 0 2) (TmVar 2 2))))
ex6_2_2_2 = TmAbs "x" (TmApp (TmApp (TmVar 0 1) (TmVar 1 1)) (TmAbs "y" (TmApp (TmApp (TmVar 0 2) (TmVar 1 2)) (TmVar 2 2))))
