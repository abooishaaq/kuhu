{-# LANGUAGE GADTs #-}

module Syntax where

import Type ( Type )

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
    App :: String -> [Expr] -> Expr
    Assign :: Expr -> Expr -> Expr
    Array :: [Expr] -> Expr
    Index :: Expr -> Expr -> Expr
    Range :: Expr -> Expr -> Expr
    Incr :: Expr -> Expr
    Decr :: Expr -> Expr
    deriving (Show, Eq)

data Stmt where
    Expr :: Expr -> Stmt
    Def :: String -> Expr -> Stmt
    If :: Expr -> [Stmt] -> [Stmt] -> Stmt
    For :: String -> Expr -> [Stmt] -> Stmt
    Fun :: String -> [(String, Type)] -> [Stmt] -> Stmt
    deriving (Show, Eq)

data TopLevel where
    TopLevel :: [Stmt] -> TopLevel
  deriving (Show, Eq)
