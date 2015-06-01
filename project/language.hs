type Program 	= [Clause]

data Clause	= Expression [Expression]

data Expression = String

defaultProgram :: Program
defaultProgram =  ["a0", "a1", "a2", "b0" ["a0", "a1"], "b1" ["a1", "a2"], "b2" ["a1", "a2", "d"], "c0" ["b0", "b1"], "c1" ["b0", "b1", "b2"]]
