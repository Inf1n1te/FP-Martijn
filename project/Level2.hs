{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Level2 where

-- -- -- Imports -- -- --


-- -- -- Test programs -- -- --

prog1 :: Program
prog1 = [(("a",Constant "v"),[]),(("b", Variable "A"),[("a", Variable "A")]),(("c", Variable "A"),[("b", Variable "A"),("a", Constant "v")])]

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

