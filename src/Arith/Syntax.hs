module Arith.Syntax where

type Info = (Int, Int)
dummyinfo = (-1, -1)

data Term = TmTrue Info
          | TmFalse Info
          | TmIf Info Term Term Term
          | TmZero Info
          | TmSucc Info Term
          | TmPred Info Term
          | TmIsZero Info Term
            deriving (Show)

tT :: Term
tT = TmTrue dummyinfo

tF :: Term
tF = TmFalse dummyinfo

tZ :: Term
tZ = TmZero dummyinfo

t1 :: Term
t1 = TmPred dummyinfo (TmSucc dummyinfo (TmPred dummyinfo (TmZero dummyinfo)))

t2 :: Term
t2 = TmIf dummyinfo tZ tT tF
