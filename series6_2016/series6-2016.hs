module FP_Grammar where

import FPPrac.Trees
import Data.Map

data Expr   = Const Int
            | Tuple Expr Expr
            | Triple Expr Expr Expr
            | Boolean Bool
            | Var String
            | BinOp String Expr Expr
            | If Expr Expr Expr
            | Lambda Type Expr
            | App Expr Expr
    deriving Show
data Type   = IntType 
            | TupleType Type Type
            | TripleType Type Type Type
            | BoolType
            | FunType Type Type
    deriving (Show, Eq)

--type (TupleType IntType IntType) = TupleType IntType IntType
--type (TupleType BoolType BoolType) = TupleType BoolType BoolType
--type (TripleType IntType IntType IntType) = TripleType IntType IntType IntType
--type (TripleType BoolType BoolType BoolType) = TripleType BoolType BoolType BoolType

type Env = [(String, Type)]
env :: Env
env =   [("+", FunType IntType (FunType IntType IntType))
        ,("-", FunType IntType (FunType IntType IntType))
        ,("*", FunType IntType (FunType IntType IntType))
        ,("&&", FunType BoolType (FunType BoolType BoolType))
        ,("||", FunType BoolType (FunType BoolType BoolType))
        
        ,("++", FunType (TupleType IntType IntType) (FunType (TupleType IntType IntType) (TupleType IntType IntType)))
        ,("--", FunType (TupleType IntType IntType) (FunType (TupleType IntType IntType) (TupleType IntType IntType)))
        ,("**", FunType (TupleType IntType IntType) (FunType (TupleType IntType IntType) (TupleType IntType IntType)))
        ,("&&&", FunType (TupleType BoolType BoolType) (FunType (TupleType BoolType BoolType) (TupleType BoolType BoolType)))
        ,("|||", FunType (TupleType BoolType BoolType) (FunType (TupleType BoolType BoolType) (TupleType BoolType BoolType)))

        ,("+++", FunType (TripleType IntType IntType IntType) (FunType (TripleType IntType IntType IntType) (TripleType IntType IntType IntType)))
        ,("---", FunType (TripleType IntType IntType IntType) (FunType (TripleType IntType IntType IntType) (TripleType IntType IntType IntType)))
        ,("***", FunType (TripleType IntType IntType IntType) (FunType (TripleType IntType IntType IntType) (TripleType IntType IntType IntType)))
        ,("&&&&", FunType (TripleType BoolType BoolType BoolType) (FunType (TripleType BoolType BoolType BoolType) (TripleType BoolType BoolType BoolType)))
        ,("||||", FunType (TripleType BoolType BoolType BoolType) (FunType (TripleType BoolType BoolType BoolType) (TripleType BoolType BoolType BoolType)))
        
        ,("+*&&", FunType (TripleType IntType IntType BoolType) (FunType (TripleType IntType IntType BoolType) (TripleType IntType IntType BoolType)))
        
        
        ,("x", IntType)
        ,("y", IntType)
        ,("z", IntType)

        ,("a", BoolType)
        ,("b", BoolType)
        ,("c", BoolType)
        ]
        
testTriplePlus = typeOf env (BinOp "+++" (Triple (Const 5) (Const 3) (Const 12)) (Triple (Const 22) (Var "x") (Var "y")))


typeOf :: Env -> Expr -> Type
typeOf env (Const _)        = IntType
typeOf env (Boolean _)      = BoolType
typeOf env (Var str)        = (fromList env)!str
typeOf env (Tuple e1 e2)    = TupleType (typeOf env e1) (typeOf env e2)
typeOf env (Triple e1 e2 e3)= TripleType (typeOf env e1) (typeOf env e2) (typeOf env e3)
typeOf env (BinOp op e1 e2) = case t_op of 
    FunType t0 (FunType t1 t2)
        | t0 == t_e1 && t1 == t_e2  -> t2
        | otherwise                 -> error "Types do not match"
    where
        t_op    = (fromList env)!op
        t_e1    = typeOf env e1
        t_e2    = typeOf env e2
typeOf env (Lambda t1 e2)   = FunType t1 (typeOf env e2) 
typeOf env (App e1 e2)              | typeOf env e2 == t1   = t2
                                    | otherwise             = error "Types do not match"
        where
            (FunType t1 t2) = typeOf env e1
