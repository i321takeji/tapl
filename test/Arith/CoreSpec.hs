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
         prop "small-step == big-step" prop_3_5_17

instance Arbitrary Term where
    arbitrary = sized sizedTerm

sizedTerm 0         = oneof [ liftM TmTrue arbitrary
                            , liftM TmFalse arbitrary
                            , liftM TmZero arbitrary
                            ]
sizedTerm n | n > 0 = oneof [ liftM TmTrue arbitrary
                            , liftM TmFalse arbitrary
                            , liftM4 TmIf arbitrary subterm subterm subterm
                            , liftM TmZero arbitrary
                            , liftM2 TmSucc arbitrary subterm
                            , liftM2 TmPred arbitrary subterm
                            , liftM2 TmIsZero arbitrary subterm
                            ]
            where
              subterm = sizedTerm (n `div` 2)

prop_3_5_17 t = isval vs && isval vb ==> vs == vb
    where
      vs = eval t
      vb = evalBig t
