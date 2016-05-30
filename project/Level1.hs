module Level1 where

-- Imports


-- Data types

type Atom		= String
type Clause		= (Atom, [Atom])
type Program	= [Clause]
type Query		= [Atom]


-- Functions
evalProp :: Program -> Query -> Bool
evalProp [] _ 			= False
evalProp _ []			= True
evalProp program query 	=