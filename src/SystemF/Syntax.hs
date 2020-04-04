module SystemF.Syntax
  ( Ty (..),
    Term (..),
    Binding (..),

    -- ** 型のシフトと代入
    -- $def_type_shifting
    typeShift,
    -- $def_type_substitution
    typeSubst,
  )
where

-- | de Bruijn インデックスによる表現
data Ty
  = -- | 型変数 (束縛子までの距離と期待される文脈のサイズ)
    TyVar Int Int
  | -- | 関数型
    TyArr Ty Ty
  | -- | 全称限量子
    TyAll Name Ty
  | -- | 存在限量子
    TySome Name Ty
  deriving (Eq, Show)

data Term
  = TmVar Int Int
  | TmAbs Name Ty Term
  | TmApp Term Term
  deriving (Show)

type Name = String

data Binding
  = NameBind
  | VarBind Ty
  | TyVarBind
  deriving (Show)

-- $def_type_shifting
-- __演習問題 25.2.1. [ \(\star\) ]__
--
-- \[
-- \begin{align*}
--   &\uparrow^{d}_{c}(\mathtt{k}) &=&&&
--     \begin{cases}
--       \mathtt{k}   & \mathtt{k} < c の場合    \\
--       \mathtt{k}+d & \mathtt{k} \geq c の場合
--     \end{cases}                                                           \\
--   &\uparrow^{d}_{c}(\mathtt{T}_{1} \to \mathtt{T}_{2}) &=&&&
--     \uparrow^{d}_{c}(\mathtt{T}_{1}) \to \uparrow^{d}_{c}(\mathtt{T}_{2}) \\
--   &\uparrow^{d}_{c}(\forall . \mathtt{T}_{1}) &=&&&
--     \forall . \uparrow^{d}_{c+1}(\mathtt{T}_{1})                          \\
--   &\uparrow^{d}_{c}(\{\exists, \mathtt{T}_{1}\}) &=&&&
--     \{\exists, \uparrow^{d}_{c+1}(\mathtt{T}_{1})\}
-- \end{align*}
-- \]

-- | 型のシフト (素朴な定義)
typeShift ::
  -- | 自由変数をシフトする量 d
  Int ->
  -- | それ未満はシフトしない打ち切り値 c
  Int ->
  -- | 型 \(\mathtt{T}\)
  Ty ->
  -- | \(\uparrow^{d}_{c}(\mathtt{T})\)
  Ty
typeShift d c (TyVar x n)
  | x < c = TyVar x (n + d)
  | otherwise = TyVar (x + d) (n + d)
typeShift d c (TyArr tyT1 tyT2) = TyArr (typeShift d c tyT1) (typeShift d c tyT2)
typeShift d c (TyAll tyX tyT1) = TyAll tyX (typeShift (d + 1) c tyT1)

typeShift' :: Int -> Ty -> Ty
typeShift' d tyT = walk 0 tyT
  where
    walk c (TyVar x n)
      | x >= c = TyVar (x + d) (n + d)
      | otherwise = TyVar x (n + d)
    walk c (TyArr tyT1 tyT2) = TyArr (walk c tyT1) (walk c tyT2)
    walk c (TyAll tyX tyT1) = TyAll tyX (walk (c + 1) tyT1)

-- $def_type_substitution
--
-- 型 \(\mathtt{T}\) における，型変数番号 \(\mathtt{j}\) への型 \(\mathtt{S}\) の代入を \([\mathtt{j} \mapsto \mathtt{S}]\mathtt{T}\) と書く．
-- \[
-- \begin{align*}
--   &[\mathtt{j} \mapsto \mathtt{S}](\mathtt{k}) &=&&&
--     \begin{cases}
--       \mathtt{S} & \mathtt{k} = \mathtt{j} の場合 \\
--       \mathtt{k} & それ以外の場合
--     \end{cases}                                                                                         \\
--   &[\mathtt{j} \mapsto \mathtt{S}](\mathtt{T}_{1} \to \mathtt{T}_{2}) &=&&&
--     ([\mathtt{j} \mapsto \mathtt{S}]\mathtt{T}_{1}) \to ([\mathtt{j} \mapsto \mathtt{S}]\mathtt{T}_{2}) \\
--   &[\mathtt{j} \mapsto \mathtt{S}](\forall . \mathtt{T}_{1}) &=&&&
--     \forall . [\mathtt{j}+1 \mapsto \uparrow^{1}_{0}(\mathtt{S})]\mathtt{T}_{1}                         \\
--   &[\mathtt{j} \mapsto \mathtt{S}](\{\exists , \mathtt{T}_{1}\}) &=&&&
--     \{\exists , [\mathtt{j}+1 \mapsto \uparrow^{1}_{0}(\mathtt{S})]\mathtt{T}_{1}\}
-- \end{align*}
-- \]

-- | 型の代入 (素朴な定義)
typeSubst ::
  -- | 型変数番号 \(\mathtt{j}\)
  Int ->
  -- | 型 \(\mathtt{S}\)
  Ty ->
  -- | 型 \(\mathtt{T}\)
  Ty ->
  -- | \([\mathtt{j} \mapsto \mathtt{S}]\mathtt{T}\)
  Ty
typeSubst j tyS tyT@(TyVar x n)
  | x == j = tyS
  | otherwise = tyT
typeSubst j tyS (TyArr tyT1 tyT2) = TyArr (typeSubst j tyS tyT1) (typeSubst j tyS tyT2)
typeSubst j tyS (TyAll tyX tyT1) = TyAll tyX (typeSubst (j + 1) (typeShift 1 0 tyS) tyT1)
