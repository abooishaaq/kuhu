{-# LANGUAGE GADTs #-}

module Type where

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar !TVar
  | TCon !String
  | TArrow !Type !Type
  | TArray !Type
  | TStruct ![(String, Type)]
  deriving (Show, Eq, Ord)

data Scheme = Forall ![TVar] !Type
  deriving (Show, Eq, Ord)

typeInt = TCon "Int"
typeBool = TCon "Bool"
typeFloat = TCon "Float"
typeUnit = TCon "Unit"

-- type classes
typeNum = TCon "Num"
typeOrd = TCon "Ord"
typeEq = TCon "Eq"

subtype :: Type -> Type -> Bool
subtype t1 t2 =
  case t2 of
    TCon "Num" -> case t1 of
      TCon "Int" -> True
      TCon "Float" -> True
      _ -> False
    TCon "Ord" -> case t1 of
      TCon "Int" -> True
      TCon "Float" -> True
      _ -> False
    TCon "Eq" -> case t1 of
      TCon "Int" -> True
      TCon "Float" -> True
      TCon "Bool" -> True
      _ -> False
    _ -> t1 == t2
