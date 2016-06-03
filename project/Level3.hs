{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Level3 where

-- Imports
import Data.List
import Data.Either

-- Data types
data Term           = Constant String | Variable String
    deriving (Show, Eq)
type Atom           = (String, [Term])
type Clause         = (Atom, [Atom])
type Program        = [Clause]
type Query          = [Atom]
type Substitution   = (Term, Term)

-- -- -- Test Data -- -- --
query1 :: Query
query1 = [ -- Desired output unknown
    ("p", [Variable "A", Variable "X"])
    ]

program1 :: Program
program1 = [
    (("p", [Variable "X", Variable "Y"]), 
        [("r", [Constant "b"]), ("s", [Variable "X"]), ("t", [Variable "Y"])]),
    (("q", [Variable "Y", Variable "Z"]), 
        [("r", [Variable "Z", Variable "Y"]), ("t", [Variable "Y"])]),
    (("r", [Constant "a"]), []),
    (("r", [Constant "b"]), []),
    (("r", [Constant "c"]), []),
    (("s", [Variable "Z"]), [("r", [Variable "Z"])]),
    (("s", [Constant "d"]), []),
    (("t", [Constant "b"]), []),
    (("t", [Constant "d"]), [])]



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

rename :: Program -> Query -> Program
rename program query    = foldl (<~) program (getSubstitutions program query)

getSubstitutions :: Program -> Query -> [Substitution]
getSubstitutions program query = zip (nub $ intersect programvars queryvars) (map (\x -> Variable x) newVarNames)
    where
        programvars = [x | let z = (map (fst) program) ++ (concat $ map (snd) program), let y = concat $ map (snd) z, x@(Variable _) <- y]
        queryvars   = [x | let y = concat $ map (snd) query, x@(Variable _) <- y]
        newVarNames = getNewVarNames varNames (program ++ [(("_query", [Constant "a"]), query)])



-- |getNewVarNames: Retrieves a list of strings found in [String] not found in any variable in the Program. 
getNewVarNames :: [String] -> Program -> [String]
getNewVarNames [] program            = error "No free name found in seed"
getNewVarNames (name:seed) program   | elem name (map getstr $ concat variables)  
                                        = getNewVarNames seed program 
                                    | otherwise
                                        = name : getNewVarNames seed program
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
unify (_,[]) _          = error "Cannot unify: empty atom"
unify _ (_,[])          = error "Cannot unify: empty atom"
unify atom1 atom2   
    | length (snd atom1) /= length (snd atom2)  = error "Cannot unify: atoms of different lengths"
    | otherwise                                 = unify' atom1 atom2
    where
        unify' :: Atom -> Atom -> [Substitution]
        unify' (_,[]) (_,[]) = []
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


(<?>) :: Atom -> Atom -> Bool
(_,[]) <?> _          = False
_ <?> (_,[])          = False
atom1 <?> atom2   
    | length (snd atom1) /= length (snd atom2)  = False
    | otherwise                                 = all (==True) $ unify' atom1 atom2
    where
        unify' :: Atom -> Atom -> [Bool]
        unify' (_,[]) (_,[]) = []
        unify' (predicate1, (term1Head@(Constant _):term1Tail)) (predicate2, (term2Head@(Constant _):term2Tail))
            | predicate1 /= predicate2  = [False]
            | term1Head == term2Head    = True : (unify' atom1 atom2)
            | otherwise                 = [False]
            where
                unification             = (term1Head, term2Head)
                atom1                   = (predicate1, term1Tail) <~ unification
                atom2                   = (predicate2, term2Tail) <~ unification
        unify' (predicate1, (term1Head@(Variable _):term1Tail)) (predicate2, (term2Head@(Constant _):term2Tail))
            | predicate1 == predicate2  = True : (unify' atom1 atom2)
            | otherwise                 = [False]
            where
                unification             = (term1Head, term2Head)
                atom1                   = (predicate1, term1Tail) <~ unification
                atom2                   = (predicate2, term2Tail) <~ unification
        unify' (predicate1, (term1Head@(Constant _):term1Tail)) (predicate2, (term2Head@(Variable _):term2Tail))
            | predicate1 == predicate2  = True : (unify' atom1 atom2)
            | otherwise                 = [False]
            where
                unification             = (term2Head, term1Head)
                atom1                   = (predicate1, term1Tail) <~ unification
                atom2                   = (predicate2, term2Tail) <~ unification
        unify' (predicate1, (term1Head@(Variable _):term1Tail)) (predicate2, (term2Head@(Variable _):term2Tail))
            | predicate1 == predicate2  = True : (unify' atom1 atom2)
            | otherwise                 = [False]
            where
                unification             = (term2Head, term1Head)
                atom1                   = (predicate1, term1Tail) <~ unification
                atom2                   = (predicate2, term2Tail) <~ unification

-- evalMulti wrapper function
evalMulti :: Program -> Query -> [Either Bool [Substitution]]
evalMulti [] _              = error "Empty program"
evalMulti _ []              = error "Empty query"
evalMulti program query     | null $ rightsRes  = filter (isLeft) res
                            | otherwise         = rightsRes {-= res-}
    where
        res = eval (rename program query) query

        trimmed = trim res
        rightsRes = filter (isRight) trimmed
        vars = [x | let y = concat $ map (snd) query, x@(Variable _) <- y]
        trim :: [Either Bool [Substitution]] -> [Either Bool [Substitution]]
        trim []             = []
        trim ((Right x):xs) | null $ trim' x    = trim xs
                            | otherwise         = Right (trim' x) : trim xs
        trim ((Left x):xs)  = trim xs

        trim' :: [Substitution] -> [Substitution]
        trim' [] = []
        trim' (x@(term1@(Variable _), term2@(Variable _)):xs)
            | elem term1 vars && elem term2 vars    = x : (trim' xs)
            | otherwise                             = trim' xs
        trim' (x@(term1@(Variable _), (Constant _)):xs)
            | elem term1 vars   = x : (trim' xs)
            | otherwise         = trim' xs
        trim' (_:xs)            = trim' xs

-- eval function
eval :: Program -> Query -> [Either Bool [Substitution]]
eval [] _ = [Left False]
eval _ [] = [Left True]
eval program query@(queryAtomHead:queryAtoms)
    | null res = [Left False]
    | otherwise = foldr (\(f,s) a -> [f] ++ ( s) ++ a) [] res
    where
        res = [(Right unification, evals)|
            clause@(clauseAtom, clauseAtoms) <- program,
        
--            trace ("query: "++ (show queryAtomHead) ++ " -> " ++ (show queryAtoms) ++ " rule: " ++ (show clauseAtom) ++ " -> "++ (show clauseAtoms))
        
            queryAtomHead <?> clauseAtom,
            let unification = unify queryAtomHead clauseAtom,
            let evals = eval program ((foldl (<~) clauseAtoms unification) ++ (foldl (<~) queryAtoms unification)), 
            evals /= [Left False]
            ]
