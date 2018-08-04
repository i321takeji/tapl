module Untyped.Syntax
    (NamelessTerm (..), Context, Binding (..), showNTm, termShift, termSubst, termSubstTop)
where

data NamelessTerm = NTmVar Int Int                    -- ^ 自由変数と文脈の長さ
                  | NTmAbs Name NamelessTerm          -- ^ 第 1 引数は，元の束縛変数名（のヒント）
                  | NTmApp NamelessTerm NamelessTerm
            deriving (Eq, Show)

type Context = [(Name, Binding)]
type Name    = String
data Binding = NameBind
               deriving (Show)

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
index2name ctx x = fst $ ctx !! x

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
