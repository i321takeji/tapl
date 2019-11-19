module Recon.Syntax where

import Data.List (findIndex)

data Ty = TyNat
        | TyBool
        | TyArr Ty Ty
        | TyId Name
          deriving (Eq, Show)

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          | TmVar Name
          | TmAbs Name Ty Term        -- ^ 第 1 引数は，元の束縛変数名（のヒント）
          | TmApp Term Term
            deriving (Show)

data NamelessTerm = NTmTrue
                  | NTmFalse
                  | NTmIf NamelessTerm NamelessTerm NamelessTerm
                  | NTmZero
                  | NTmSucc NamelessTerm
                  | NTmPred NamelessTerm
                  | NTmIsZero NamelessTerm
                  | NTmVar Int Int             -- ^ 自由変数と文脈の長さ
                  | NTmAbs Name Ty NamelessTerm        -- ^ 第 1 引数は，元の束縛変数名（のヒント）
                  | NTmApp NamelessTerm NamelessTerm
                    deriving (Show)

type Context = [(Name, Binding)]
type Name    = String
data Binding = NameBind
             | VarBind Ty
               deriving (Show)

addBinding :: Context -> Name -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx

getTypeFromContext :: Context -> Int -> Ty
getTypeFromContext ctx i = case getBinding ctx i of
  VarBind ty -> ty
  _          -> error $ "getTypeFromContext: Wrong kind of binding for variable " ++ index2name ctx i

getBinding :: Context -> Int -> Binding
getBinding ctx i = snd $ ctx !! i

index2name :: Context -> Int -> Name
index2name ctx x = fst $ ctx !! x

name2index :: Context -> Name -> Int
name2index ctx x = case findIndex ((==x) . fst) ctx of
                     Just k  -> k
                     Nothing -> error "[bad name]"

isNameBound :: Context -> Name -> Bool
isNameBound ctx x = x `elem` fst (unzip ctx)

pickFreshName :: Context -> Name -> (Context, Name)
pickFreshName ctx x | isNameBound ctx x = pickFreshName ctx (x ++ "'")
                    | otherwise         = ((x, NameBind):ctx, x)

removenames :: Context -> Term -> NamelessTerm
removenames ctx TmTrue            = NTmTrue
removenames ctx TmFalse           = NTmFalse
removenames ctx (TmIf t1 t2 t3)   = NTmIf (removenames ctx t1) (removenames ctx t2) (removenames ctx t3)
removenames ctx TmZero            = NTmZero
removenames ctx (TmSucc t1)       = NTmSucc (removenames ctx t1)
removenames ctx (TmPred t1)       = NTmPred (removenames ctx t1)
removenames ctx (TmIsZero t1)     = NTmIsZero (removenames ctx t1)
removenames ctx (TmVar x)         = NTmVar (name2index ctx x) (length ctx)
removenames ctx (TmAbs x tyT1 t1) = NTmAbs x' tyT1 (removenames ctx' (rename x x' t1))
  where
    (ctx', x') = pickFreshName ctx x
removenames ctx (TmApp t1 t2)     = NTmApp (removenames ctx t1) (removenames ctx t2)

rename :: Name -> Name -> Term -> Term
rename x y v@(TmVar x')
    | x == x'            = TmVar y
    | otherwise          = v
rename x y abs@(TmAbs x' tyT1 t1)
    | x == x'            = abs
    | otherwise          = TmAbs x' tyT1 (rename x y t1)
rename x y (TmApp t1 t2) = TmApp (rename x y t1) (rename x y t2)

typeof :: Context -> NamelessTerm -> Ty
typeof _   NTmTrue            = TyBool
typeof _   NTmFalse           = TyBool
typeof ctx (NTmIf t1 t2 t3)
  | typeof ctx t1 == TyBool   = let tyT2 = typeof ctx t2
                               in  if tyT2 == typeof ctx t3 then tyT2
                                   else error "arms of conditional have different types"
  | otherwise                 = error "guard of conditional not a boolean"
typeof _   NTmZero            = TyNat
typeof ctx (NTmSucc t1)
  | typeof ctx t1 == TyNat    = TyNat
  | otherwise                 = error "couldn't match type"
typeof ctx (NTmPred t1)
  | typeof ctx t1 == TyNat    = TyNat
  | otherwise                 = error "couldn't match type"
typeof ctx (NTmIsZero t1)
  | typeof ctx t1 == TyNat    = TyBool
  | otherwise                 = error "couldn't match type"
typeof ctx (NTmVar i c)       = getTypeFromContext ctx i
typeof ctx (NTmAbs x tyT1 t2) = TyArr tyT1 tyT2
  where
    ctx' = addBinding ctx x (VarBind tyT1)
    tyT2 = typeof ctx' t2
typeof ctx (NTmApp t1 t2)     = case tyT1 of
                                  TyArr tyT11 tyT12 -> if tyT11 == tyT2 then tyT12
                                                       else error "parameter type mismatch"
                                  _                 -> error "arrow type expected"
  where
    tyT1 = typeof ctx t1
    tyT2 = typeof ctx t2

-- Exercise 22.3.10
type Constr = [(Ty, Ty)]
data NextUVar = NextUVar String (() -> NextUVar)

uvargen :: Int -> NextUVar
uvargen n = NextUVar ("?X_" ++ show n) (\() -> uvargen (n+1))

recon :: Context -> NextUVar -> NamelessTerm -> (Ty, NextUVar, Constr)
recon _   nextuvar NTmTrue            = (TyBool, nextuvar, [])
recon _   nextuvar NTmFalse           = (TyBool, nextuvar, [])
recon ctx nextuvar (NTmIf t1 t2 t3)   = (tyT2, nextuvar3, c')
  where
    (tyT1, nextuvar1, c1) = recon ctx nextuvar t1
    (tyT2, nextuvar2, c2) = recon ctx nextuvar1 t2
    (tyT3, nextuvar3, c3) = recon ctx nextuvar2 t3
    c' = c1 ++ c2 ++ c3 ++ [(tyT1, TyBool), (tyT2, tyT3)]
recon _   nextuvar NTmZero            = (TyNat, nextuvar, [])
recon ctx nextuvar (NTmSucc t1)       = (TyNat, nextuvar1, c' ++ [(tyT, TyNat)]) 
  where
    (tyT, nextuvar1, c') = recon ctx nextuvar t1
recon ctx nextuvar (NTmPred t1)       = (TyNat, nextuvar1, c' ++ [(tyT, TyNat)]) 
  where
    (tyT, nextuvar1, c') = recon ctx nextuvar t1
recon ctx nextuvar (NTmIsZero t1)     = (TyBool, nextuvar1, c' ++ [(tyT, TyNat)]) 
  where
    (tyT, nextuvar1, c') = recon ctx nextuvar t1
recon ctx nextuvar (NTmVar i _)       = (getTypeFromContext ctx i, nextuvar, [])
recon ctx nextuvar (NTmAbs x tyT1 t2) = (TyArr tyT1 tyT2, nextuvar1, c')
  where
    ctx' = addBinding ctx x (VarBind tyT1)
    (tyT2, nextuvar1, c') = recon ctx' nextuvar t2
recon ctx nextuvar (NTmApp t1 t2)     = (tyIdX, nextuvar' (), c')
  where
    (tyT1, nextuvar1, c1) = recon ctx nextuvar t1
    (tyT2, nextuvar2, c2) = recon ctx nextuvar1 t2
    NextUVar tyX nextuvar' = nextuvar2
    tyIdX = TyId tyX
    c' = c1 ++ c2 ++ [(tyT1, TyArr tyT2 tyIdX)]

doRecon :: Term -> (Ty, Constr)
doRecon = result . recon [] (uvargen 1) . removenames []
  where
    result (tyT, _, c) = (tyT, c)

{-
doRecon :: NamelessTerm -> (Ty, Constr)
doRecon = result . recon [] (uvargen 1)
  where
    result (tyT, _, c) = (tyT, c)
-}

exer22_2_3 = TmAbs "x" (TyId "X")
               (TmAbs "y" (TyId "Y")
                 (TmAbs "z" (TyId "Z")
                   (TmApp (TmApp (TmVar "x") (TmVar "z")) (TmApp (TmVar "y") (TmVar "z")))))
