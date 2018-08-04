module Untyped.SyntaxSpec where

import Untyped.Syntax

import Control.Monad

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

instance Arbitrary NamelessTerm where
    arbitrary = sized sizedTerm

sizedTerm 0         = NTmVar <$> genNat <*> genNat
sizedTerm n | n > 0 = frequency [ (1, NTmVar <$> genNat <*> genNat)
                                , (2, NTmAbs <$> genName <*> sizedTerm (n-1))
                                , (4, do a <- choose (0, n-1)
                                         NTmApp <$> sizedTerm a <*> sizedTerm (n-1-a))
                                ]
            where
              genName = do v <- choose ('a', 'z')
                           return [v]

--            genName = do h <- elements ['a'..'z']
--                         t <- listOf (elements (['a'..'z'] ++ ['A'..'Z']))
--                         return (h:t)

genNat = choose (0, 20 :: Int)


spec :: Spec
spec = do
  describe "show term" $ do
         it "'4 (3 2)' should be 'x (y z)'" $ do
                showNTm p77_gamma p77_01 `shouldBe` "(x (y z))"

         it "'\\. 4 0' should be '\\w. y w'" $ do
                showNTm p77_gamma p77_02 `shouldBe` "(\\w. (y w))"

         it "'\\.\\. 6' should be '\\w. \\a'. x'" $ do
                showNTm p77_gamma p77_03 `shouldBe` "(\\w. (\\a'. x))"

  describe "Shifting" $ do
         it "ex6.2.2 (1) '↑ 2 (\\.\\. 1 (0 2))' should be '(\\.\\. 1 (0 4))'" $ do
                termShift 2 ex6_2_2_1 `shouldBe` NTmAbs "x" (NTmAbs "y" (NTmApp (NTmVar 1 4) (NTmApp (NTmVar 0 4) (NTmVar 4 4))))

         it "ex6.2.2 (2) '↑ 2 (\\. 0 1 (\\. 0 1 2))' should be '(\\. 0 3 (\\. 0 1 4))'" $ do
                termShift 2 ex6_2_2_2 `shouldBe` NTmAbs "x" (NTmApp (NTmApp (NTmVar 0 3) (NTmVar 3 3)) (NTmAbs "y" (NTmApp (NTmApp (NTmVar 0 4) (NTmVar 1 4)) (NTmVar 4 4))))

  describe "Substitution" $ do
         it "ex6.2.5 (1) '[0 |-> 1] (0 (\\.\\. 2))' should be '1 (\\.\\. 3)'" $ do
                termSubst ex6_2_5_1_j ex6_2_5_1_s ex6_2_5_1_t `shouldBe` NTmApp (NTmVar 1 2) (NTmAbs "x" (NTmAbs "y" (NTmVar 3 4)))

         it "ex6.2.5 (2) '[0 |-> 1 (\\. 2)] (0 (\\. 1))' should be '(1 (\\. 2)) (\\. 2 (\\. 3))'" $ do
                termSubst ex6_2_5_2_j ex6_2_5_2_s ex6_2_5_2_t `shouldBe` NTmApp (NTmApp (NTmVar 1 2) (NTmAbs "z" (NTmVar 2 3))) (NTmAbs "x" (NTmApp (NTmVar 2 3) (NTmAbs "z" (NTmVar 3 4))))

         it "ex6.2.5 (3) '[0 |-> 1] (\\. 0 2)' should be '\\. (0 2)'" $ do
                termSubst ex6_2_5_3_j ex6_2_5_3_s ex6_2_5_3_t `shouldBe` NTmAbs "b" (NTmApp (NTmVar 0 3) (NTmVar 2 3))

         it "ex6.2.5 (4) '[0 |-> 1] (\\. 1 0)' should be '\\. (2 0)'" $ do
                termSubst ex6_2_5_4_j ex6_2_5_4_s ex6_2_5_4_t `shouldBe` NTmAbs "a" (NTmApp (NTmVar 2 3) (NTmVar 0 3))

--p77_gamma = zip ["x", "y", "z", "a", "b"] (repeat NameBind)
p77_gamma = zip ["b", "a", "z", "y", "x"] (repeat NameBind)
p77_01 = NTmApp (NTmVar 4 5) (NTmApp (NTmVar 3 5) (NTmVar 2 5))
p77_02 = NTmAbs "w" (NTmApp (NTmVar 4 6) (NTmVar 0 6))
p77_03 = NTmAbs "w" (NTmAbs "a" (NTmVar 6 7))

ex6_2_2_1 = NTmAbs "x" (NTmAbs "y" (NTmApp (NTmVar 1 2) (NTmApp (NTmVar 0 2) (NTmVar 2 2))))
ex6_2_2_2 = NTmAbs "x" (NTmApp (NTmApp (NTmVar 0 1) (NTmVar 1 1)) (NTmAbs "y" (NTmApp (NTmApp (NTmVar 0 2) (NTmVar 1 2)) (NTmVar 2 2))))

ex6_2_5_gamma = zip ["b", "a"] (repeat NameBind)
ex6_2_5_1_j = 0
ex6_2_5_1_s = NTmVar 1 2
ex6_2_5_1_t = NTmApp (NTmVar 0 2) (NTmAbs "x" (NTmAbs "y" (NTmVar 2 4)))
ex6_2_5_2_j = 0
ex6_2_5_2_s = NTmApp (NTmVar 1 2) (NTmAbs "z" (NTmVar 2 3))
ex6_2_5_2_t = NTmApp (NTmVar 0 2) (NTmAbs "x" (NTmVar 1 3))
ex6_2_5_3_j = 0
ex6_2_5_3_s = NTmVar 1 2
ex6_2_5_3_t = NTmAbs "b" (NTmApp (NTmVar 0 3) (NTmVar 2 3))
ex6_2_5_4_j = 0
ex6_2_5_4_s = NTmVar 1 2
ex6_2_5_4_t = NTmAbs "a" (NTmApp (NTmVar 1 3) (NTmVar 0 3))
