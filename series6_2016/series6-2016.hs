module FP_Grammar where

import FPPrac.Trees
import Data.Map

data Expr   = Const Int
            | Boolean Bool
            | Var String
            | BinOp String Expr Expr
            | App Expr Expr
    deriving Show
data Type   = IntType 
            | BoolType
            | FunType Type Type
    deriving (Show, Eq)

type Env = [(String, Type)]
env :: Env
env =   [("+", FunType IntType (FunType IntType IntType))
        ,("-", FunType IntType (FunType IntType IntType))
        ,("*", FunType IntType (FunType IntType IntType))

        ,("&&", FunType BoolType (FunType BoolType BoolType))
        ,("||", FunType BoolType (FunType BoolType BoolType))

        ,("x", IntType)
        ,("y", IntType)
        ,("z", IntType)
        ]


typeOf :: Env -> Expr -> Type
typeOf env (Const _)        = IntType
typeOf env (Boolean _)      = BoolType
typeOf env (Var str)        = (fromList env)!str
typeOf env (BinOp op e1 e2) = case t_op of FunType t0 (FunType t1 t2)
        | t0 == t_e1 && t1 == t_e2  -> t2
        | otherwise                 -> error "Types do not match"
    where
        t_op    = (fromList env)!op
        t_e1    = typeOf env e1
        t_e2    = typeOf env e2
