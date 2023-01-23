{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Infer where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Containers.ListUtils (nubOrd)
import Env
import Syntax
import Type

-- | Inference monad
type Infer a =
    ( ReaderT
        Env -- Typing environment
        ( StateT -- Inference state
            InferState
            ( Except -- Inference errors
                TypeError
            )
        )
        a -- Result
    )

-- | Inference state
newtype InferState = InferState {count :: Int}

-- | Initial inference state
initInfer :: InferState
initInfer = InferState{count = 0}

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

newtype Subst = Subst (Map.Map TVar Type)
    deriving (Eq, Ord, Show)

instance Semigroup Subst where
    Subst s1 <> Subst s2 = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

instance Monoid Subst where
    mempty = Subst Map.empty

class Substitutable a where
    apply :: Subst -> a -> a
    ftv :: a -> Set.Set TVar

instance Substitutable Type where
    apply _ (TCon a) = TCon a
    apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
    apply s (t1 `TArrow` t2) = apply s t1 `TArrow` apply s t2
    apply s (TArray t) = TArray $ apply s t

    ftv TCon{} = Set.empty
    ftv (TVar a) = Set.singleton a
    ftv (t1 `TArrow` t2) = ftv t1 `Set.union` ftv t2
    ftv (TArray t) = ftv t

instance Substitutable Scheme where
    apply (Subst s) (Forall as t) = Forall as $ apply s' t
      where
        s' = Subst $ foldr Map.delete s as
    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
    apply s (t1, t2) = (apply s t1, apply s t2)
    ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable Env where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env

data TypeError
    = UnificationFail !Type !Type
    | InfiniteType !TVar !Type
    | UnboundVariable !String
    | Ambigious ![Constraint]
    | UnificationMismatch ![Type] ![Type]

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Env -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: Env -> Expr -> Either TypeError Scheme
inferExpr env ex = case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) -> case runSolve cs of
        Left err -> Left err
        Right subst -> Right $ closeOver $ apply subst ty

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Env -> Expr -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env ex = case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) -> case runSolve cs of
        Left err -> Left err
        Right subst -> Right (cs, subst, ty, sc)
          where
            sc = closeOver $ apply subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize Env.empty

-- | Extend type environment
inEnv :: (String, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
    let scope e = remove e x `extend` (x, sc)
    local scope m

-- | Lookup type in the environment
lookupEnv :: String -> Infer Type
lookupEnv x = do
    (TypeEnv env) <- ask
    case Map.lookup x env of
        Nothing -> throwError $ UnboundVariable x
        Just s -> instantiate s

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ Map.fromList $ zip as as'
    return $ apply s t

generalize :: Env -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

arithOps :: ArithOp -> Type
arithOps Plus = typeInt `TArrow` (typeInt `TArrow` typeInt)
arithOps Times = typeInt `TArrow` (typeInt `TArrow` typeInt)
arithOps Minus = typeInt `TArrow` (typeInt `TArrow` typeInt)
arithOps Divide = typeInt `TArrow` (typeInt `TArrow` typeInt)
arithOps Remainder = typeInt `TArrow` (typeInt `TArrow` typeInt)

boolOps :: BoolOp -> Type
boolOps And = typeBool `TArrow` (typeBool `TArrow` typeBool)
boolOps Or = typeBool `TArrow` (typeBool `TArrow` typeBool)

bitOps :: BitOp -> Type
bitOps Andb = typeInt `TArrow` (typeInt `TArrow` typeInt)
bitOps Orb = typeInt `TArrow` (typeInt `TArrow` typeInt)
bitOps Xor = typeInt `TArrow` (typeInt `TArrow` typeInt)
bitOps Shl = typeInt `TArrow` (typeInt `TArrow` typeInt)
bitOps Shr = typeInt `TArrow` (typeInt `TArrow` typeInt)

-- | Infer the type of an expression
infer :: Expr -> Infer (Type, [Constraint])
infer (Var x) = do
    t <- lookupEnv x
    return (t, [])
infer (Int _) = return (typeInt, [])
infer (Bool _) = return (typeBool, [])
infer (Float _) = return (typeFloat, [])
infer (Negate e) = do
    (t, cs) <- infer e
    return (t, (t, typeInt) : cs)
infer (Not e) = do
    (t, cs) <- infer e
    return (t, (t, typeBool) : cs)
infer (Assign x e) = do
    (t1, cs1) <- infer e
    (t2, cs2) <- infer x
    return (t1, (t1, t2) : cs1 ++ cs2)
infer (ArithOp op e1 e2) = do
    (t1, cs1) <- infer e1
    (t2, cs2) <- infer e2
    t <- fresh
    let u1 = t1 `TArrow` (t2 `TArrow` t)
        u2 = arithOps op
    return (t, (u1, u2) : cs1 ++ cs2)
infer (BoolOp op e1 e2) = do
    (t1, cs1) <- infer e1
    (t2, cs2) <- infer e2
    t <- fresh
    let u1 = t1 `TArrow` (t2 `TArrow` t)
        u2 = boolOps op
    return (t, (u1, u2) : cs1 ++ cs2)
infer (BitOp op e1 e2) = do
    (t1, cs1) <- infer e1
    (t2, cs2) <- infer e2
    t <- fresh
    let u1 = t1 `TArrow` (t2 `TArrow` t)
        u2 = bitOps op
    return (t, (u1, u2) : cs1 ++ cs2)
infer (App var args) = do
    env <- ask
    (fnty, cs1) <- infer var
    (argtys, cs2) <- unzip <$> mapM infer args
    retty <- fresh
    let u1 = foldr TArrow retty argtys
        u2 = fnty
    return (retty, (u1, u2) : cs1 ++ concat cs2)
infer CmpOp{} = return (typeBool, [])
infer (Index ex1 ex2) = do
    (t1, cs1) <- infer ex1
    (t2, cs2) <- infer ex2
    t <- fresh
    return (t, (t1, TArray t) : (t2, typeInt) : cs1 ++ cs2)
infer (Range ex1 ex2) = do
    (t1, cs1) <- infer ex1
    (t2, cs2) <- infer ex2
    return (TArray typeInt, (t1, typeInt) : (t2, typeInt) : cs1 ++ cs2)
infer (Incr ex) = do
    (t, cs) <- infer ex
    return (t, (t, typeInt) : cs)
infer (Decr ex) = do
    (t, cs) <- infer ex
    return (t, (t, typeInt) : cs)
infer (Array exs) = do
    (ts, css) <- unzip <$> mapM infer exs
    t <- fresh
    let cs = map (t,) ts
    return (TArray t, cs ++ concat css)

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nubOrd $ fv body) (map TV letters)

    fv (TVar a) = [a]
    fv (TArrow a b) = fv a ++ fv b
    fv (TArray a) = fv a
    fv (TCon _) = []

    normtype (TArray a) = TArray (normtype a)
    normtype (TArrow a b) = TArrow (normtype a) (normtype b)
    normtype (TCon a) = TCon a
    normtype (TVar a) =
        case Prelude.lookup a ord of
            Just x -> TVar x
            Nothing -> error "type variable not in signature"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where
    st = (emptySubst, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
    do
        su1 <- unifies t1 t2
        su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
        return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArrow t1 t2) (TArrow t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
    case cs of
        [] -> return su
        ((t1, t2) : cs0) -> do
            su1 <- unifies t1 t2
            solver (su1 `compose` su, apply su1 cs0)

bind :: TVar -> Type -> Solve Subst
bind a t
    | t == TVar a = return emptySubst
    | occursCheck a t = throwError $ InfiniteType a t
    | otherwise = return (Subst $ Map.singleton a t)

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t
