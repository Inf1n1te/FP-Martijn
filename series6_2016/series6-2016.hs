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
    deriving Show

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
typeOf env (Var str)        = map!str
    where
        map = fromList env
typeOf env (BinOp str _ _)  = map!str
    where
        map = fromList env
