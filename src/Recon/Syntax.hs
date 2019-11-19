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
          | TmVar Int Int             -- ^ 自由変数と文脈の長さ
          | TmAbs Name Ty Term        -- ^ 第 1 引数は，元の束縛変数名（のヒント）
          | TmApp Term Term
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

typeof :: Context -> Term -> Ty
typeof _   TmTrue            = TyBool
typeof _   TmFalse           = TyBool
typeof ctx (TmIf t1 t2 t3)
  | typeof ctx t1 == TyBool  = let tyT2 = typeof ctx t2
                               in  if tyT2 == typeof ctx t3 then tyT2
                                   else error "arms of conditional have different types"
  | otherwise                = error "guard of conditional not a boolean"
typeof _   TmZero            = TyNat
typeof ctx (TmSucc t1)
  | typeof ctx t1 == TyNat   = TyNat
  | otherwise                = error "couldn't match type"
typeof ctx (TmPred t1)
  | typeof ctx t1 == TyNat   = TyNat
  | otherwise                = error "couldn't match type"
typeof ctx (TmIsZero t1)
  | typeof ctx t1 == TyNat   = TyBool
  | otherwise                = error "couldn't match type"
typeof ctx (TmVar i c)       = getTypeFromContext ctx i
typeof ctx (TmAbs x tyT1 t2) = TyArr tyT1 tyT2
  where
    ctx' = addBinding ctx x (VarBind tyT1)
    tyT2 = typeof ctx' t2
typeof ctx (TmApp t1 t2)     = case tyT1 of
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

recon :: Context -> NextUVar -> Term -> (Ty, NextUVar, Constr)
recon _   nextuvar TmTrue            = (TyBool, nextuvar, [])
recon _   nextuvar TmFalse           = (TyBool, nextuvar, [])
recon ctx nextuvar (TmIf t1 t2 t3)   = (tyT2, nextuvar3, c')
  where
    (tyT1, nextuvar1, c1) = recon ctx nextuvar t1
    (tyT2, nextuvar2, c2) = recon ctx nextuvar1 t2
    (tyT3, nextuvar3, c3) = recon ctx nextuvar2 t3
    c' = c1 ++ c2 ++ c3 ++ [(tyT1, TyBool), (tyT2, tyT3)]
recon _   nextuvar TmZero            = (TyNat, nextuvar, [])
recon ctx nextuvar (TmSucc t1)       = (TyNat, nextuvar1, c' ++ [(tyT, TyNat)]) 
  where
    (tyT, nextuvar1, c') = recon ctx nextuvar t1
recon ctx nextuvar (TmPred t1)       = (TyNat, nextuvar1, c' ++ [(tyT, TyNat)]) 
  where
    (tyT, nextuvar1, c') = recon ctx nextuvar t1
recon ctx nextuvar (TmIsZero t1)     = (TyBool, nextuvar1, c' ++ [(tyT, TyNat)]) 
  where
    (tyT, nextuvar1, c') = recon ctx nextuvar t1
recon ctx nextuvar (TmVar i _)       = (getTypeFromContext ctx i, nextuvar, [])
recon ctx nextuvar (TmAbs x tyT1 t2) = (TyArr tyT1 tyT2, nextuvar1, c')
  where
    ctx' = addBinding ctx x (VarBind tyT1)
    (tyT2, nextuvar1, c') = recon ctx' nextuvar t2
recon ctx nextuvar (TmApp t1 t2)     = (tyIdX, nextuvar' (), c')
  where
    (tyT1, nextuvar1, c1) = recon ctx nextuvar t1
    (tyT2, nextuvar2, c2) = recon ctx nextuvar1 t2
    NextUVar tyX nextuvar' = nextuvar2
    tyIdX = TyId tyX
    c' = c1 ++ c2 ++ [(tyT1, TyArr tyT2 tyIdX)]

doRecon :: Term -> (Ty, Constr)
doRecon = result . recon [] (uvargen 1)
  where
    result (tyT, _, c) = (tyT, c)

expr = TmAbs "x" (TyId "X")
         (TmAbs "y" (TyId "Y")
            (TmAbs "z" (TyId "Z") (TmVar 2 999)))

exer22_2_3 = TmAbs "x" (TyId "X")
               (TmAbs "y" (TyId "Y")
                 (TmAbs "z" (TyId "Z")
                   (TmApp (TmApp (TmVar 2 999) (TmVar 0 999)) (TmApp (TmVar 1 999) (TmVar 0 999)))))
