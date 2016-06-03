{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Level3 where

-- Imports
import Data.List

-- Data types
data Term           = Constant String | Variable String
    deriving (Show, Eq)
type Atom           = (String, [Term])
type Clause         = (Atom, [Atom])
type Program        = [Clause]
type Query          = [Atom]
type Substitution   = (Term, Term)


-- Functions



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
    term <~ (original@(Constant _), replacement)
        | term == original && original == replacement   = replacement
        | original == replacement                       = term
        | otherwise = error "Cannot substitute a constant"

instance Substitute [Term] where
    -- Substitute in a list of Terms
    terms <~ substitution = map (<~ substitution) terms

instance Substitute Atom where
    -- Substitute a variable with a Term
    (predicate, terms) <~ substitution = (predicate, terms <~ substitution)

instance Substitute [Atom] where
     -- Substitute a variable with a Term for all variables in a list of atoms
    atoms <~ substitution = map (<~ substitution) atoms

instance Substitute Clause where
    -- Substitute a variable with a Term for all variables in a clause
    (atom, atoms) <~ substitution 
        = (atom <~ substitution, atoms <~ substitution)

instance Substitute Program where
    -- Substitute in a program
    program <~ substitution = map (<~ substitution) program

-- -- -- -- - - -- -- -- --
-- -- Rename function -- --
-- -- -- -- - - -- -- -- --

getSubstitutions :: Program -> Query -> [Substitution]
getSubstitutions program query = zip (intersect programvars queryvars) newVarNames
    where
        programvars = 
        queryvars   = [x | let y = concat $ map (snd) query, x@(Variable _) <- y]
        newVarNames = getNewVarNames varNames (program ++ [(("_query", Constant "a"), query)])



-- |getNewVarNames: Retrieves a list of strings found in [String] not found in any variable in the Program. 
getNewVarNames :: [String] -> Program -> [String]
getNewVarNames [] program            = error "No free name found in seed"
getNewVarNames (name:seed) program   | elem name (map getstr variables)  
                                        = getNewVarNames seed program 
                                    | otherwise
                                        = name ++ getNewVarNames seed program
    where
        getstr (Variable str) = str
        getstr (Constant str) = str
        variables   = concat $ map clausevars program
        clausevars  = (\((name, variable),atoms) -> variable : (map atomvars atoms) )
        atomvars    = (\(name, variable) -> variable)

-- |varNames: Generates an infinite list like ["A","B",..,"AA","AB",..]
varNames :: [String] 
varNames = [empty ++ [abc] | empty <- "" : varNames, abc <- ['A'..'Z']]

{- Unify function
Finds a substitution to unify two atoms.

Usage:
unify Atom Atom
-}
unify :: Atom -> Atom -> [Substitution]
unify [] _          = error "Cannot unify: empty atom"
unify _ []          = error "Cannot unify: empty atom"
unify atom1 atom2   
    | len (snd atom1) /= len (snd atom2)    = error "Cannot unify: atoms of different lengths"
    | otherwise                             = unify' atom atom


unify' :: Atom -> Atom -> [Substitution]
unify' [] [] = []
unify' (predicate1, (term1Head@(Constant _):term1Tail)) (predicate2, (term2Head@(Constant _):term2Tail))
    | predicate1 /= predicate2  = error "Cannot unify: nonequal predicates"
    | term1Head == term2Head    = unification : (unify' atom1 atom2)
    | otherwise                 = error "Cannot unify: nonequal constants"
    where
        unification             = (term1Head, term2Head)
        atom1                   = (predicate1, term1Tail) <~ unification
        atom2                   = (predicate2, term2Tail) <~ unification
unify' (predicate1, (term1Head@(Variable _):term1Tail)) (predicate2, (term2Head@(Constant _):term2Tail))
    | predicate1 == predicate2  = unification : (unify' atom1 atom2)
    | otherwise                 = error "Cannot unify: nonequal predicates"
    where
        unification             = (term1Head, term2Head)
        atom1                   = (predicate1, term1Tail) <~ unification
        atom2                   = (predicate2, term2Tail) <~ unification
unify' (predicate1, (term1Head@(Constant _):term1Tail)) (predicate2, (term2Head@(Variable _):term2Tail))
    | predicate1 == predicate2  = unification : (unify' atom1 atom2)
    | otherwise                 = error "Cannot unify: nonequal predicates"
    where
        unification             = (term2Head, term1Head)
        atom1                   = (predicate1, term1Tail) <~ unification
        atom2                   = (predicate2, term2Tail) <~ unification
unify' (predicate1, (term1Head@(Variable _):term1Tail)) (predicate2, (term2Head@(Variable _):term2Tail))
    | predicate1 == predicate2  = unification : (unify' atom1 atom2)
    | otherwise                 = error "Cannot unify: nonequal predicates"
    where
        unification             = (term2Head, term1Head)
        atom1                   = (predicate1, term1Tail) <~ unification
        atom2                   = (predicate2, term2Tail) <~ unification
