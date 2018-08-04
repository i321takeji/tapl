module Untyped.Core
    (eval)
where

import Untyped.Syntax

isVal :: Context -> NamelessTerm -> Bool
isVal ctx (NTmAbs _ _) = True
isVal _   _            = False

-- | single-step evaluation
eval1 :: Context -> NamelessTerm -> Maybe NamelessTerm
eval1 ctx (NTmApp (NTmAbs x t12) v2) | isVal ctx v2 = return $ termSubstTop v2 t12
eval1 ctx (NTmApp v1 t2) | isVal ctx v1             = do t2' <- eval1 ctx t2
                                                         return $ NTmApp v1 t2'
eval1 ctx (NTmApp t1 t2)                            = do t1' <- eval1 ctx t1
                                                         return $ NTmApp t1' t2
eval1 _ _                                           = Nothing

-- | multi-step evaluation
eval :: Context -> NamelessTerm -> NamelessTerm
eval ctx t = case eval1 ctx t of
               Just t' -> eval ctx t'
               Nothing -> t
