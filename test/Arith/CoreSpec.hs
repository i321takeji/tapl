module Arith.CoreSpec where

import Arith.Syntax
import Arith.Core

import Control.Monad

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "small-step == big-step" $ do
         prop "(small-step t ->* v ==> big-step t ↓↓ v) && (big-step t ↓↓ v ==> small-step t ->* v)" prop_ex3_5_17

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

-- | exercise 3.5.17: t ->* v <=> t ↓↓ v
prop_ex3_5_17 t = prop_ex3_5_17_forward t .&&. prop_ex3_5_17_backward t

prop_ex3_5_17_forward t = let vs =  eval t
                          in isval vs ==> vs == evalBig t

prop_ex3_5_17_backward t = let vb =  evalBig t
                           in isval vb ==> eval t == vb
