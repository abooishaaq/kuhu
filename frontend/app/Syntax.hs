{-# LANGUAGE GADTs #-}

module Syntax where

import Type (Type)

data ArithOp = Plus | Minus | Times | Divide | Remainder
    deriving (Show, Eq)

data BitOp = Andb | Orb | Xor | Shl | Shr
    deriving (Show, Eq)

data BoolOp = And | Or
    deriving (Show, Eq)

data CmpOp = Less | More | Equal | Unequal
    deriving (Show, Eq)

data Expr where
    Var :: String -> Expr
    Bool :: Bool -> Expr
    Int :: Int -> Expr
    Float :: Double -> Expr
    Not :: Expr -> Expr
    Negate :: Expr -> Expr
    ArithOp :: ArithOp -> Expr -> Expr -> Expr
    BoolOp :: BoolOp -> Expr -> Expr -> Expr
    CmpOp :: CmpOp -> Expr -> Expr -> Expr
    BitOp :: BitOp -> Expr -> Expr -> Expr
    App :: Expr -> [Expr] -> Expr
    Assign :: Expr -> Expr -> Expr
    ArrayBuilder :: Expr -> Expr -> Expr
    Array :: [Expr] -> Expr
    Index :: Expr -> Expr -> Expr
    Range :: Expr -> Expr -> Expr
    StructEx :: String -> [(String, Expr)] -> Expr
    Access :: () -> Expr -> Expr -> Expr
    Incr :: Expr -> Expr
    Decr :: Expr -> Expr
    deriving (Show, Eq)

data Stmt where
    Expr :: Expr -> Stmt
    Def :: String -> Expr -> Stmt
    If :: Expr -> [Stmt] -> [Stmt] -> Stmt
    For :: String -> Expr -> [Stmt] -> Stmt
    Return :: Expr -> Stmt
    deriving (Show, Eq)

data Struct where
    Struct :: String -> [(String, Type)] -> Struct
    deriving (Show, Eq)

data Impl where
    Impl :: String -> [Fun] -> Impl
    deriving (Show, Eq)

data Fun where
    Fun :: String -> [String] -> [Type] -> Type -> [Stmt] -> Fun
    deriving (Show, Eq)

data TopLevel where
    TopLevelStruct :: Struct -> TopLevel
    TopLevelImpl :: Impl -> TopLevel
    TopLevelFun :: Fun -> TopLevel
    deriving (Show, Eq)

type Program = [TopLevel]
