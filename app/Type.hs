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
typeUnit = TCon "Unit"
