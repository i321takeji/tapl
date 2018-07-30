module Untyped.Syntax
    ()
where

data Term = TmVar Int Int
          | TmAbs Name Term
          | TmApp Term Term
            deriving (Eq, Show)

type Context = [(Name, Binding)]
type Name    = String
data Binding = NameBind
               deriving (Show)

showTm :: Context -> Term -> String
showTm ctx (TmAbs x t1)  = "(\\ " ++ x' ++ ". " ++ showTm ctx' t1 ++ ")"
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

--gamma = zip ["x", "y", "z", "a", "b"] (repeat NameBind)
gamma = zip ["b", "a", "z", "y", "x"] (repeat NameBind)
t0 = TmVar 4 5
t1 = TmApp (TmVar 4 5) (TmApp (TmVar 3 5) (TmVar 2 5))
