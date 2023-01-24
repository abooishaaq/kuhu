{-# LANGUAGE GADTs #-}

module Check where

import Env
import Infer
import Syntax
import Type

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Prelude hiding (lookup)

type TExpr1 = (Type, Expr)

data TExpr where
    TVar :: String -> TExpr
    TBool :: Bool -> TExpr
    TInt :: Int -> TExpr
    TFloat :: Double -> TExpr
    TNot :: TExpr1 -> TExpr
    TNegate :: TExpr1 -> TExpr
    TArithOp :: ArithOp -> TExpr1 -> TExpr1 -> TExpr
    TBoolOp :: BoolOp -> TExpr1 -> TExpr1 -> TExpr
    TCmpOp :: CmpOp -> TExpr1 -> TExpr1 -> TExpr
    TBitOp :: BitOp -> TExpr1 -> TExpr1 -> TExpr
    TApp :: TExpr1 -> [TExpr1] -> TExpr
    TAssign :: TExpr1 -> TExpr1 -> TExpr
    TArrayBuilder :: TExpr1 -> TExpr1 -> TExpr
    TArray :: [TExpr] -> TExpr
    TIndex :: TExpr1 -> TExpr1 -> TExpr
    TRange :: TExpr1 -> TExpr1 -> TExpr
    TStructEx :: String -> [(String, TExpr)] -> TExpr
    TAccess :: () -> TExpr1 -> TExpr1 -> TExpr
    TIncr :: TExpr1 -> TExpr
    TDecr :: TExpr1 -> TExpr
    deriving (Show, Eq)

funty :: Fun -> Type
funty (Fun _ _ argsty retty _) = foldr TArrow retty argsty

funName :: Fun -> String
funName (Fun name _ _ _ _) = name

structAddField :: Type -> String -> Type -> Type
structAddField (TStruct fields) name ty = TStruct ((name, ty) : fields)
structAddField _ _ _ = error "what the heck?"

toplevelTys :: Env -> [TopLevel] -> Infer Env -- ReaderT becomes redundant
toplevelTys env [] = return env
toplevelTys env (t : ts) = case t of
    TopLevelStruct (Struct name fields) ->
        let env1 = extend env (name, generalize env (TStruct fields))
         in toplevelTys env1 ts
    TopLevelImpl (Impl name funs) -> do
        let env1 = env `remove` name
        let structsc = lookup name env
        case structsc of
            Just sc -> do
                structty <- instantiate sc
                let funtys = map funty funs
                let structy = foldl (\ty (name, ty') -> structAddField ty name ty') structty (zip (map funName funs) funtys)
                let env2 = extend env1 (name, generalize env1 structy)
                toplevelTys env2 ts
            _ -> throwError $ UnboundVariable name
    TopLevelFun fun -> do
        let name = funName fun
        let env1 = env `remove` name
        let ty = funty fun
        toplevelTys (extend env1 (name, generalize env1 ty)) ts

runTopLevelTys :: [TopLevel] -> Either TypeError Env
runTopLevelTys toplevel = runExcept $ evalStateT (runReaderT (toplevelTys empty toplevel) empty) initInfer
