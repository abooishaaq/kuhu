{-# LANGUAGE GADTs #-}

module Type where

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar !TVar
  | TCon !String
  | TArrow !Type !Type
  | TArray !Type
  | TStruct1 !String
  | TStruct2 ![(String, Type)]
  deriving (Show, Eq, Ord)

data Scheme = Forall ![TVar] !Type
  deriving (Show, Eq, Ord)

typeInt  = TCon "Int"
typeBool = TCon "Bool"
typeFloat = TCon "Float"

match :: Type -> Type -> Bool
match (TVar _) _ = True
match (TCon a) (TCon b) = a == b
match (TArrow a b) (TArrow c d) = match a c && match b d
match (TArray a) (TArray b) = match a b
match _ _ = False

matchsc :: Scheme -> Scheme -> Bool
matchsc (Forall as t) (Forall bs u) = match t u && length as == length bs
