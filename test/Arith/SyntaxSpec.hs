module Arith.SyntaxSpec where

import Arith.Syntax

import Control.Monad

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = describe "Arith.Syntax" $ do
         it "declared Term as an instance of Arbitrary" $ print "Syntax has no test."

instance Arbitrary Term where
    arbitrary = sized sizedTerm

sizedTerm 0         = oneof [ pure TmTrue
                            , pure TmFalse
                            , pure TmZero
                            ]
sizedTerm n | n > 0 = frequency [ (1, pure TmTrue)
                                , (1, pure TmFalse)
                                , (4, do a <- choose (0, n-1)
                                         b <- choose (0, n-1-a)
                                         TmIf <$> sizedTerm a <*> sizedTerm b <*> sizedTerm (n-1-a-b))
                                 , (1, pure TmZero)
                                 , (2, TmSucc <$> subterm)
                                 , (2, TmPred <$> subterm)
                                 , (2, TmIsZero <$> subterm)
                                 ]
            where
              subterm = sizedTerm (n-1)
