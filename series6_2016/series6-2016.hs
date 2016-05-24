module FP_Grammar where

import FPPrac.Trees

data Expr   = Const Int
            | Var String
            | BinOp String Expr Expr
            | App Expr Expr
data Type   = IntType 
            | FunType Type Type

type Env = [(String, Type)]

env :: Env
env =   [("+", FunType IntType (FunType IntType IntType))
        ,("-", FunType IntType (FunType IntType IntType))
        ,("*", FunType IntType (FunType IntType IntType))

        ,("x", IntType)
        ]


typeOf :: Env -> Expr -> Type
typeOf env expr 