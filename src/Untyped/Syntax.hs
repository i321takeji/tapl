module Untyped.Syntax
    (NamelessTerm (..), Context, Binding (..), showNTm, termShift, termSubst, termSubstTop)
where

import Data.List (findIndex, delete)
import Data.Set (Set, singleton, (\\), union)

data Term = TmVar Name
          | TmAbs Name Term
          | TmApp Term Term
            deriving (Eq, Show)

data NamelessTerm = NTmVar Int Int                    -- ^ 自由変数と文脈の長さ
                  | NTmAbs Name NamelessTerm          -- ^ 第 1 引数は，元の束縛変数名（のヒント）
                  | NTmApp NamelessTerm NamelessTerm
                    deriving (Show)

instance Eq (NamelessTerm) where
    (NTmVar k n) == (NTmVar k' n')     = k == k'
    (NTmAbs _ t1) == (NTmAbs _ t1')    = t1 == t1'
    (NTmApp t1 t2) == (NTmApp t1' t2') = t1 == t1' && t2 == t2'

type Context = [(Name, Binding)]
type Name    = String
data Binding = NameBind
               deriving (Show)

removenames :: Context -> Term -> NamelessTerm
removenames ctx (TmVar x)     = NTmVar (name2index ctx x) (length ctx)
removenames ctx (TmAbs x t1)  = NTmAbs x' (removenames ctx' (rename x x' t1))
    where (ctx', x') = pickFreshName ctx x
removenames ctx (TmApp t1 t2) = NTmApp (removenames ctx t1) (removenames ctx t2)

freevars :: Term -> Set Name
freevars (TmVar x)     = singleton x
freevars (TmAbs x t1)  = freevars t1 \\ singleton x
freevars (TmApp t1 t2) = freevars t1 `union` freevars t2

rename :: Name -> Name -> Term -> Term
rename x y v@(TmVar x')
    | x == x'            = TmVar y
    | otherwise          = v
rename x y abs@(TmAbs x' t1)
    | x == x'            = abs
    | otherwise          = TmAbs x' (rename x y t1)
rename x y (TmApp t1 t2) = TmApp (rename x y t1) (rename x y t2)

subst :: Name -> Term -> Term -> Term
subst x s v@(TmVar y)
    | x == y              = s
    | otherwise           = v
subst x s abs@(TmAbs y t1)
    | x == y              = abs
    | y `elem` freevars s = subst x s (rename y (y++ "'") abs)
    | otherwise           = TmAbs y (subst x s t1)
subst x y (TmApp t1 t2)   = TmApp (subst x y t1) (subst x y t2)

showNTm :: Context -> NamelessTerm -> String
showNTm ctx (NTmAbs x t1)  = "(\\" ++ x' ++ ". " ++ showNTm ctx' t1 ++ ")"
    where (ctx', x') = pickFreshName ctx x
showNTm ctx (NTmApp t1 t2) = "(" ++ showNTm ctx t1 ++ " " ++ showNTm ctx t2 ++ ")"
showNTm ctx (NTmVar x n)   = if length ctx == n then index2name ctx x
                             else error "[bad index]"

printNTm :: Context -> NamelessTerm -> IO ()
printNTm ctx t = putStrLn $ showNTm ctx t

isNameBound :: Context -> Name -> Bool
isNameBound ctx x = x `elem` fst (unzip ctx)

pickFreshName :: Context -> Name -> (Context, Name)
pickFreshName ctx x | isNameBound ctx x = pickFreshName ctx (x ++ "'")
                    | otherwise         = ((x, NameBind):ctx, x)

index2name :: Context -> Int -> Name
index2name ctx k = fst $ ctx !! k

name2index :: Context -> Name -> Int
name2index ctx x = case findIndex ((==x) . fst) ctx of
                     Just k  -> k
                     Nothing -> error "[bad name]"

termShift :: Int -> NamelessTerm -> NamelessTerm
termShift d t = walk 0 t
    where
      walk c (NTmVar k n) | k < c     = NTmVar k     (n+d)
                          | otherwise = NTmVar (k+d) (n+d)
      walk c (NTmAbs x t1)            = NTmAbs x (walk (c+1) t1)
      walk c (NTmApp t1 t2)           = NTmApp (walk c t1) (walk c t2)

termSubst :: Int -> NamelessTerm -> NamelessTerm -> NamelessTerm
termSubst j s t = walk 0 t
    where
      walk c k'@(NTmVar k n) | k == j+c  = termShift c s
                             | otherwise = k'
      walk c    (NTmAbs x t1)            = NTmAbs x (walk (c+1) t1)
      walk c    (NTmApp t1 t2)           = NTmApp (walk c t1) (walk c t2)

termSubstTop :: NamelessTerm -> NamelessTerm -> NamelessTerm
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)
