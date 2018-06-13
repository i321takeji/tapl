module Core where

import Arith.Syntax

isnumericval :: Term -> Bool
isnumericval (TmZero _)     = True
isnumericval (TmSucc _ t1)  = isnumericval t1
isnumericval _              = False

isval :: Term -> Bool
isval (TmTrue _)         = True
isval (TmFalse _)        = True
isval t | isnumericval t = True
isval _                  = False

eval1 :: Term -> Maybe Term
eval1 (TmIf _ (TmTrue _) t2 t3)   = Just t2
eval1 (TmIf _ (TmFalse _) t2 t3)  = Just t3
eval1 (TmIf fi t1 t2 t3)          = let (Just t1') = eval1 t1
                                    in Just $ TmIf fi t1' t2 t3
eval1 (TmSucc fi t1)              = let (Just t1') = eval1 t1
                                    in Just $ TmSucc fi t1'
eval1 (TmPred _ (TmZero _))       = Just $ TmZero dummyinfo
eval1 (TmPred _ (TmSucc _ nv1))
    | isnumericval nv1            = Just nv1
eval1 (TmPred fi t1)              = let (Just t1') = eval1 t1
                                    in Just $ TmPred fi t1'
eval1 (TmIsZero _ (TmZero _))     = Just $ TmTrue dummyinfo
eval1 (TmIsZero _ (TmSucc _ nv1))
    | isnumericval nv1            = Just $ TmFalse dummyinfo
eval1 (TmIsZero fi t1)            = let (Just t1') = eval1 t1
                                    in Just $ TmIsZero fi t1'
eval1 _                           = Nothing

eval :: Term -> Term
eval t = case eval1 t of
           Just t' -> eval t'
           Nothing -> t

-- eval1 の Applicative 版
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
