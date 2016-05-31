{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Level2 where

-- -- -- Imports -- -- --


-- -- -- Data types -- -- --
data Term           = Constant String | Variable String
    deriving (Show, Eq)
type Atom           = (String, Term)
type Clause         = (Atom, [Atom])
type Program        = [Clause]
type Query          = [Atom]
type Substitution   = (Term, Term)

-- Substitution operation in type classes
class Substitute a where
    (<~) :: a -> Substitution -> a

instance Substitute Term where
    term <~ (original@(Variable _), replacement)
        | term == original  = replacement
        | otherwise         = term
    _ <~ ((Constant _), _)
        = error "Cannot substitute a constant"

instance Substitute Atom where
    atom@(predicate, term) <~ (original@(Variable _), replacement)
        | term == original  = (predicate, replacement)
        | otherwise         = atom
    _ <~ ((Constant _), _)
        = error "Cannot substitute a constant"

-- -- -- Functions -- -- --

-- Rename function
rename :: Program -> Query -> Program
rename 


varNames :: [String] 
varNames = [empty ++ [abc] | empty <- "" : varNames, abc <- ['A'..'Z']]


-- Unify function
unify :: Atom -> Atom -> Substitution
unify (firstPredicate, firstConstant@(Constant _)) (secondPredicate, secondConstant@(Constant _))
    | firstPredicate /= secondPredicate = error "Cannot unify: nonequal predicates"
    | firstConstant == secondConstant   = error "Already unified: equal constants"
    | otherwise                         = error "Cannot unify: nonequal constants"
unify (firstPredicate, variable@(Variable _)) (secondPredicate, constant@(Constant _))
    | firstPredicate == secondPredicate = (variable, constant)
    | otherwise                         = error "Cannot unify: nonequal predicates"
unify (firstPredicate, constant@(Constant _)) (secondPredicate, variable@(Variable _))
    | firstPredicate == secondPredicate = (variable, constant)
    | otherwise                         = error "Cannot unify: nonequal predicates"
unify (firstPredicate, firstVariable@(Variable _)) (secondPredicate, secondVariable@(Variable _))
    | firstPredicate == secondPredicate = (secondVariable, firstVariable)
    | otherwise                         = error "Cannot unify: nonequal predicates"

-- evalOne function

