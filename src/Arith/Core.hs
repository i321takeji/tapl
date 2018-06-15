module Arith.Core where

import Arith.Syntax

import Control.Monad (guard)

isnumericval :: Term -> Bool
isnumericval TmZero      = True
isnumericval (TmSucc t1) = isnumericval t1
isnumericval _           = False

isval :: Term -> Bool
isval TmTrue             = True
isval TmFalse            = True
isval t | isnumericval t = True
isval _                  = False

-- | small-step evaluation
eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue t2 t3)     = return t2
eval1 (TmIf TmFalse t2 t3)    = return t3
eval1 (TmIf t1 t2 t3)         = do t1' <- eval1 t1
                                   return $ TmIf t1' t2 t3
eval1 (TmSucc t1)             = do t1' <- eval1 t1
                                   return $ TmSucc t1'
eval1 (TmPred TmZero)         = return TmZero
eval1 (TmPred (TmSucc nv1))
    | isnumericval nv1        = return nv1
eval1 (TmPred t1)             = do t1' <- eval1 t1
                                   return $ TmPred t1'
eval1 (TmIsZero TmZero)       = return TmTrue
eval1 (TmIsZero (TmSucc nv1))
    | isnumericval nv1        = return TmFalse
eval1 (TmIsZero t1)           = do t1' <- eval1 t1
                                   return $ TmIsZero t1'
eval1 _                       = Nothing

eval :: Term -> Term
eval t = case eval1 t of
           Just t' -> eval t'
           Nothing -> t

-- | eval1 の Applicative 版
eval1' :: Term -> Maybe Term
eval1' (TmIf TmTrue t2 t3)         = pure t2
eval1' (TmIf TmFalse t2 t3)        = pure t3
eval1' (TmIf t1 t2 t3)             = TmIf <$> eval1' t1 <*> pure t2 <*> pure t3
eval1' (TmSucc t1)                 = TmSucc <$> eval1 t1
eval1' (TmPred TmZero)             = pure TmZero
eval1' (TmPred (TmSucc nv1))
    | isnumericval nv1             = pure nv1
eval1' (TmPred t1)                 = TmPred <$> eval1' t1
eval1' (TmIsZero TmZero)           = pure TmTrue
eval1' (TmIsZero (TmSucc nv1))
    | isnumericval nv1             = pure TmFalse
eval1' (TmIsZero t1)               = TmIsZero <$> eval1' t1
eval1' _                           = Nothing


-- big-step evaluation
type Value = Term

evalBig' :: Term -> Maybe Value
evalBig' t | isval t     = return t
evalBig' (TmIf t1 t2 t3) = do v1 <- evalBig' t1
                              case v1 of
                                TmTrue  -> evalBig' t2
                                TmFalse -> evalBig' t3
                                _       -> Nothing
evalBig' (TmSucc t1)     = do nv1 <- evalBig' t1
                              guard $ isnumericval nv1
                              return $ TmSucc nv1
evalBig' (TmPred t1)     = do nv <- evalBig' t1
                              case nv of
                                TmZero               -> return TmZero
                                TmSucc nv1
                                  | isnumericval nv1 -> return nv1
                                _                    -> Nothing
evalBig' (TmIsZero t1)   = do nv <- evalBig' t1
                              case nv of
                                TmZero               -> return TmTrue
                                TmSucc nv1
                                  | isnumericval nv1 -> return TmFalse
                                _                    -> Nothing
evalBig' _               = Nothing

evalBig :: Term -> Term
evalBig t = case evalBig' t of
              Just v  -> v
              Nothing -> t
