module Untyped.Syntax
    (Term (..), Binding (..), showTm, termShift)
where

data Term = TmVar Int Int    -- ^ 自由変数と文脈の長さ
          | TmAbs Name Term  -- ^ 第 1 引数は，元の束縛変数名（のヒント）
          | TmApp Term Term
            deriving (Eq, Show)

type Context = [(Name, Binding)]
type Name    = String
data Binding = NameBind
               deriving (Show)

showTm :: Context -> Term -> String
showTm ctx (TmAbs x t1)  = "(\\" ++ x' ++ ". " ++ showTm ctx' t1 ++ ")"
    where (ctx', x') = pickFreshName ctx x
showTm ctx (TmApp t1 t2) = "(" ++ showTm ctx t1 ++ " " ++ showTm ctx t2 ++ ")"
showTm ctx (TmVar x n)   = if length ctx == n then index2name ctx x
                           else error "[bad index]"

printTm :: Context -> Term -> IO ()
printTm ctx (TmAbs x t1)  = do let (ctx', x') = pickFreshName ctx x
                               putStr "(lambda "
                               putStr x'
                               putStr ". "
                               printTm ctx' t1
                               putStr ")"
printTm ctx (TmApp t1 t2) = do putStr "("
                               printTm ctx t1
                               putStr " "
                               printTm ctx t2
                               putStr ")"
printTm ctx (TmVar x n)   = if length ctx == n then putStr (index2name ctx x)
                            else print "[bad index]"

isNameBound :: Context -> Name -> Bool
isNameBound ctx x = x `elem` fst (unzip ctx)

pickFreshName :: Context -> Name -> (Context, Name)
pickFreshName ctx x | isNameBound ctx x = pickFreshName ctx (x ++ "'")
                    | otherwise         = ((x, NameBind):ctx, x)

index2name :: Context -> Int -> Name
index2name ctx x = fst $ ctx !! x

termShift :: Int -> Term -> Term
termShift d t = walk 0 t
    where
      walk c (TmVar k n) | k < c     = TmVar k     (n+d)
                         | otherwise = TmVar (k+d) (n+d)
      walk c (TmAbs x t1)            = TmAbs x (walk (c+1) t1)
      walk c (TmApp t1 t2)           = TmApp (walk c t1) (walk c t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = walk 0 t
    where
      walk c k'@(TmVar k n) | k == j+c  = termShift c s
                            | otherwise = k'
      walk c    (TmAbs x t1)            = TmAbs x (walk (c+1) t1)
      walk c    (TmApp t1 t2)           = TmApp (walk c t1) (walk c t2)
