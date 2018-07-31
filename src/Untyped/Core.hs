module Untyped.Core
    (eval)
where

import Untyped.Syntax

isVal :: Context -> Term -> Bool
isVal ctx (TmAbs _ _) = True
isVal _   _           = False

-- | single-step evaluation
eval1 :: Context -> Term -> Maybe Term
eval1 ctx (TmApp (TmAbs x t12) v2) | isVal ctx v2 = return $ termSubstTop v2 t12
eval1 ctx (TmApp v1 t2) | isVal ctx v1            = do t2' <- eval1 ctx t2
                                                       return $ TmApp v1 t2'
eval1 ctx (TmApp t1 t2)                           = do t1' <- eval1 ctx t1
                                                       return $ TmApp t1' t2
eval1 _ _                                         = Nothing

-- | multi-step evaluation
eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
               Just t' -> eval ctx t'
               Nothing -> t
