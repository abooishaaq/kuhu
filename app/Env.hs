{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Env where

import Prelude hiding (lookup)

import Syntax
import Type

import Data.Foldable hiding (toList)
import qualified Data.Map as Map
import Data.Monoid

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

data Env = TypeEnv {types :: Map.Map String Scheme}
    deriving (Eq, Show)

empty :: Env
empty = TypeEnv Map.empty

extend :: Env -> (String, Scheme) -> Env
extend env (x, s) = env{types = Map.insert x s (types env)}

remove :: Env -> String -> Env
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

extends :: Env -> [(String, Scheme)] -> Env
extends env xs = env{types = Map.union (Map.fromList xs) (types env)}

lookup :: String -> Env -> Maybe Scheme
lookup key (TypeEnv tys) = Map.lookup key tys

merge :: Env -> Env -> Env
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' merge empty

singleton :: String -> Scheme -> Env
singleton x y = TypeEnv (Map.singleton x y)

keys :: Env -> [String]
keys (TypeEnv env) = Map.keys env

fromList :: [(String, Scheme)] -> Env
fromList xs = TypeEnv (Map.fromList xs)

toList :: Env -> [(String, Scheme)]
toList (TypeEnv env) = Map.toList env

instance Semigroup Env where
    (<>) :: Env -> Env -> Env
    (<>) = merge
    

instance Monoid Env where
    mempty :: Env
    mempty = empty
    mappend :: Env -> Env -> Env
    mappend = merge
