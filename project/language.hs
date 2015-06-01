type Program 	= [Clause]

data Clause	= Expression [Expression]
	deriving (Eq, Show)

data Expression = String
	deriving (Eq, Show)



evalProp :: Program -> Program -> Expression -> Bool
evalProp p (c@(e n):cs) q	| e == q && n = []	= True
				| e == q 		= all (== True) $ map (evalProp p p) n
				| otherwise		= evalProp p cs e
