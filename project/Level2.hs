module Level2 where

-- Imports


-- Data types
data Term           = Constant String | Variable String
    deriving (Show, Eq)
type Atom           = (String, Term)
type Clause         = (Atom, [Atom])
type Program        = [Clause]
type Query          = [Atom]
type Substitution   = (Term, Term)

-- Substitution operation in type classes

-- Functions

-- Rename function
rename :: Program -> Query -> Program
rename 


varnames :: [String] 

-- Unify function

-- evalOne function

