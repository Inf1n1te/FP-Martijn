type Program 	= [Clause]

data Clause	= Expression (Maybe Predicate)

type Predicate	= [Expression]

type Target	= Expression

data Expression = String



