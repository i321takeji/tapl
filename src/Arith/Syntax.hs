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

t1 :: Term
t1 = TmPred dummyinfo (TmSucc dummyinfo (TmPred dummyinfo (TmZero dummyinfo)))
