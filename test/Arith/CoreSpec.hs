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
         prop "small-step == big-step" prop_ex3_5_17

instance Arbitrary Term where
    arbitrary = sized sizedTerm

sizedTerm 0         = oneof [ return TmTrue
                            , return TmFalse
                            , return TmZero
                            ]
sizedTerm n | n > 0 = frequency [ (1, return TmTrue)
                                , (1, return TmFalse)
                                , (4, liftM3 TmIf subterm subterm subterm)
                                , (1, return TmZero)
                                , (2, liftM TmSucc subterm)
                                , (2, liftM TmPred subterm)
                                , (2, liftM TmIsZero subterm)
                                ]
            where
              subterm = sizedTerm (n `div` 2)

-- | exercise 3.5.17: t ->* v <=> t ↓↓ v
prop_ex3_5_17 t = isval vs && isval vb ==> vs == vb
    where
      vs = eval t
      vb = evalBig t

sizedTerm' 0         = oneof [ pure TmTrue
                             , pure TmFalse
                             , pure TmZero
                             ]
sizedTerm' n | n > 0 = frequency [ (1, pure TmTrue)
                                 , (1, pure TmFalse)
                                 , (4, TmIf <$> subterm <*> subterm <*> subterm)
                                 , (1, pure TmZero)
                                 , (2, TmSucc <$> subterm)
                                 , (2, TmPred <$> subterm)
                                 , (2, TmIsZero <$> subterm)
                                 ]
            where
              subterm = sizedTerm' (n `div` 2)
