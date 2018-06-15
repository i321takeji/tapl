module Arith.Syntax where

--type Info = (Int, Int)
--dummyinfo = (-1, -1)

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
            deriving (Eq, Show)

tT :: Term
tT = TmTrue

tF :: Term
tF = TmFalse

tZ :: Term
tZ = TmZero

t1 :: Term
t1 = TmPred (TmSucc (TmPred TmZero))

t2 :: Term
t2 = TmIf tZ tT tF
