module Core where

import Arith.Syntax

import Control.Monad (guard)

isnumericval :: Term -> Bool
isnumericval (TmZero _)     = True
isnumericval (TmSucc _ t1)  = isnumericval t1
isnumericval _              = False

isval :: Term -> Bool
isval (TmTrue _)         = True
isval (TmFalse _)        = True
isval t | isnumericval t = True
isval _                  = False

-- | small-step evaluation
eval1 :: Term -> Maybe Term
eval1 (TmIf _ (TmTrue _) t2 t3)   = return t2
eval1 (TmIf _ (TmFalse _) t2 t3)  = return t3
eval1 (TmIf fi t1 t2 t3)          = do t1' <- eval1 t1
                                       return $ TmIf fi t1' t2 t3
eval1 (TmSucc fi t1)              = do t1' <- eval1 t1
                                       return $ TmSucc fi t1'
eval1 (TmPred _ (TmZero _))       = return $ TmZero dummyinfo
eval1 (TmPred _ (TmSucc _ nv1))
    | isnumericval nv1            = return nv1
eval1 (TmPred fi t1)              = do t1' <- eval1 t1
                                       return $ TmPred fi t1'
eval1 (TmIsZero _ (TmZero _))     = return $ TmTrue dummyinfo
eval1 (TmIsZero _ (TmSucc _ nv1))
    | isnumericval nv1            = return $ TmFalse dummyinfo
eval1 (TmIsZero fi t1)            = do t1' <- eval1 t1
                                       return $ TmIsZero fi t1'
eval1 _                           = Nothing

eval :: Term -> Term
eval t = case eval1 t of
           Just t' -> eval t'
           Nothing -> t

-- | eval1 の Applicative 版
eval1' :: Term -> Maybe Term
eval1' (TmIf _ (TmTrue _) t2 t3)   = pure t2
eval1' (TmIf _ (TmFalse _) t2 t3)  = pure t3
eval1' (TmIf fi t1 t2 t3)          = TmIf <$> pure fi <*> eval1' t1 <*> pure t2 <*> pure t3
eval1' (TmSucc fi t1)              = TmSucc <$> pure fi <*> eval1 t1
eval1' (TmPred _ (TmZero _))       = TmZero <$> pure dummyinfo
eval1' (TmPred _ (TmSucc _ nv1))
    | isnumericval nv1             = pure nv1
eval1' (TmPred fi t1)              = TmPred <$> pure fi <*> eval1' t1
eval1' (TmIsZero _ (TmZero _))     = TmTrue <$> pure dummyinfo
eval1' (TmIsZero _ (TmSucc _ nv1))
    | isnumericval nv1             = TmFalse <$> pure dummyinfo
eval1' (TmIsZero fi t1)            = TmIsZero <$> pure fi <*> eval1' t1
eval1' _                           = Nothing

-- big-step evaluation
type Value = Term

evalBig' :: Term -> Maybe Value
evalBig' t | isval t       = return t
evalBig' (TmIf _ t1 t2 t3) = do v1 <- evalBig' t1
                                case v1 of
                                  TmTrue _  -> evalBig' t2
                                  TmFalse _ -> evalBig' t3
                                  _         -> Nothing
evalBig' (TmSucc fi t1)    = do nv1 <- evalBig' t1
                                guard $ isnumericval nv1
                                return $ TmSucc fi nv1
evalBig' (TmPred fi t1)    = do nv <- evalBig' t1
                                case nv of
                                  TmZero _               -> return $ TmZero dummyinfo
                                  TmSucc _ nv1
                                      | isnumericval nv1 -> return nv1
                                  _                      -> Nothing
evalBig' (TmIsZero fi t1)  = do nv <- evalBig' t1
                                case nv of
                                  TmZero _               -> return $ TmTrue dummyinfo
                                  TmSucc _ nv1
                                      | isnumericval nv1 -> return $ TmFalse dummyinfo
                                  _                      -> Nothing
evalBig' _                 = Nothing

evalBig :: Term -> Term
evalBig t = case evalBig' t of
              Just v  -> v
              Nothing -> t
