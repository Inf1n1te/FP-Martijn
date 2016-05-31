module Level1 where

import Debug.Trace

-- Imports


-- Data types

data Term       = Atom String
    deriving (Show, Eq)
type Clause     = (Term, [Term])
type Program    = [Clause]
type Query      = [Term]


-- Test programs

program1 = [(Atom "a", []), 
            (Atom "b", [Atom "c", Atom "d"]), 
            (Atom "b", [Atom "c", Atom "e"]), 
            (Atom "c", []),
            (Atom "d", [Atom "f"]),
            (Atom "e", [])]
            
program2 = [(Atom "a", []), 
            (Atom "b", [Atom "c", Atom "d"]), 
            (Atom "b", [Atom "c", Atom "e"]), 
            (Atom "c", [Atom "d"]),
            (Atom "d", [Atom "f"]),
            (Atom "e", [])]

-- Functions
evalProp :: Program -> Query -> Bool
evalProp [] _           = False
evalProp _ []           = True
evalProp program query@(queryAtomHead:queryAtoms) | res == [] = False
                                                  | otherwise = True
    where 
        res = [ True
            | 
            clause@(clauseAtom, clauseAtoms) <- program,
        
            {-trace ("query: "++ (show queryAtomHead) ++ " -> " ++ (show queryAtoms) ++ " rule: " ++ (show clauseAtom) ++ " -> "++ (show clauseAtoms))-}
        
            clauseAtom == queryAtomHead,
            evalProp program (clauseAtoms ++ queryAtoms) ]
        
        
