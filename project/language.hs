type Program 	= [Clause]

type Clause	= Expression [Expression]

type Expression = String

defaultProgram :: Program
defaultProgram =  [a0, a1, a2, b0 [a0, a1], b1 [a1, a2], b2 [a1, a2, d], c0 [b0, b1], c1 [b0, b1, b2]]

evalProp :: Program -> Program -> Expression -> Bool
evalProp [] e _			= error "Empty base program"
evalProp [] _ _			= False
evalProp _ [] _			= False
evalProp p (c@(e, n):cs) q	| e == q && n == []	= True
				| e == q 		= all (== True) $ map (evalProp p p) n
				| otherwise		= evalProp p cs q
