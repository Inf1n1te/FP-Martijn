module Level1 where

-- Imports


-- Data types

type Atom		= String
type Clause		= (Atom, [Atom])
type Program	= [Clause]
type Query		= [Atom]


-- Functions