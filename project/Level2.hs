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

{- Substitution operation in type classes

Usage:
Term/Atom <~ Substitution

-}
class Substitute a where
    (<~) :: a -> Substitution -> a

instance Substitute Term where
    -- Substitute a variable with a Term
    term <~ (original@(Variable _), replacement)
        | term == original  = replacement
        | otherwise         = term
    -- Substitute a constant with a Term
    _ <~ ((Constant _), _)
        = error "Cannot substitute a constant"

instance Substitute Atom where
    -- Substitute a variable with a Term
    atom@(predicate, term) <~ (original@(Variable _), replacement)
        | term == original  = (predicate, replacement)
        | otherwise         = atom
    -- Substitute a constant with a Term
    _ <~ ((Constant _), _)
        = error "Cannot substitute a constant"

-- -- -- Test Data -- -- --
program1 :: Program
program1 = [
    (("p", Variable "X"), [("r", Constant "b"), ("s", Variable "X")]),
    (("q", Variable "Y"), [("r", Variable "Y"), ("t", Variable "Y")]),
    (("r", Constant "a"), []),
    (("r", Constant "b"), []),
    (("r", Constant "c"), []),
    (("s", Variable "Z"), [("r", Variable "Z")]),
    (("s", Constant "d"), []),
    (("t", Constant "b"), []),
    (("t", Constant "d"), [])]

query1 :: Query
query1 = [ -- Desired output unknown
    ("p", Variable "X"),
    ("q", Variable "Y"),
    ("s", Variable "Z"),
    ("p", Constant "a")]


-- -- -- Functions -- -- --




-- Rename function
rename :: Program -> Query -> Program
rename program []       = program
rename program query    = init $ foldl renameByAtom (program ++ [(("_query", Constant "a"), query)]) query
-- TODO: include query as new clause to program and feed to renamebyatom, and use it to fold over program. Afterwards, do not forget to move 

renameByAtom :: Program -> Atom -> Program
renameByAtom program atom@(_, oldterm@(Variable _)) = 
    map (renameClause (oldterm, (Variable newstr))) program
        where
            newstr = getNewVarName varNames program
            
            renameClause :: Substitution -> Clause -> Clause
            renameClause subst (atom, atoms) = (atom <~ subst, map (<~ subst) atoms)
renameByAtom program _ = program

getNewVarName :: [String] -> Program -> String
getNewVarName [] program            = error "No free name found in seed"
getNewVarName (name:seed) program   | elem name (map getstr variables)  
                                        = getNewVarName seed program 
                                    | otherwise
                                        = name
    where
        getstr (Variable str) = str
        getstr (Constant str) = str
        variables   = concat $ map clausevars program
        clausevars  = (\((name, variable),atoms) -> variable : (map atomvars atoms) )
        atomvars    = (\(name, variable) -> variable)

varNames :: [String] 
varNames = [empty ++ [abc] | empty <- "" : varNames, abc <- ['A'..'Z']]


{- Unify function
Finds a substitution to unify two atoms.

Usage:
unify Atom Atom
-}
unify :: Atom -> Atom -> Substitution
-- Unification of two atoms with a constant term
unify (firstPredicate, firstConstant@(Constant _)) (secondPredicate, secondConstant@(Constant _))
    | firstPredicate /= secondPredicate = error "Cannot unify: nonequal predicates"
    | firstConstant == secondConstant   = error "Already unified: equal constants"
    | otherwise                         = error "Cannot unify: nonequal constants"
-- Unification of an atom with a variable term and an atom with a constant term
unify (firstPredicate, variable@(Variable _)) (secondPredicate, constant@(Constant _))
    | firstPredicate == secondPredicate = (variable, constant)
    | otherwise                         = error "Cannot unify: nonequal predicates"
-- Unification of an atom with a constant term and an atom with a variable term
unify (firstPredicate, constant@(Constant _)) (secondPredicate, variable@(Variable _))
    | firstPredicate == secondPredicate = (variable, constant)
    | otherwise                         = error "Cannot unify: nonequal predicates"
-- Unification of two atoms with a variable term
unify (firstPredicate, firstVariable@(Variable _)) (secondPredicate, secondVariable@(Variable _))
    | firstPredicate == secondPredicate = (secondVariable, firstVariable)
    | otherwise                         = error "Cannot unify: nonequal predicates"

-- evalOne function

