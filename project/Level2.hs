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

-- -- -- Functions -- -- --

-- Rename function
rename :: Program -> Query -> Program
rename program []       = program
rename program query    = foldl renameByAtom (program ++ [(("_query", Constant "a"), query)]) query
-- TODO: include query as new clause to program and feed to renamebyatom, and use it to fold over program. Afterwards, do not forget to move 

renameByAtom :: Program -> Atom -> Program
renameByAtom program atom = 
    where
        name = foldl (getNewVarName varNames) program
        
        renameClause :: Clause -> (String, String) -> Clause
        renameClause (atom, atoms) transform = (renameAtom)
        renameAtom :: Atom -> (String, String) -> Atom
        renameAtom

getNewVarName :: [String] -> Program -> String
getNewVarName [] program            = error "No free name found in seed"
getNewVarName (name:seed) program   | elem name (map getstr variables)  
                                        = getNewVarName program seed
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

-- evalOne function

