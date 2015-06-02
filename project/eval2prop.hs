module Eval2prop where

data Argument 	= Variable String
				| Constant String
	deriving (Eq,Show)

type Program = [Clause]
type Clause = (Expression, [Expression])
type Expression = (String, Argument)

defaultProgram :: Program
defaultProgram = [
	(("p", Constant "a"), []),
	(("p", Constant "b"), []),
	(("p", Constant "c"), []),

	(("q", Constant "a"), []),
	(("q", Constant "b"), []),

	(("r", Variable "X"), [("p", Variable "X"), ("q", Variable "X")])
	]
evalOne :: Program -> Program -> Expression -> Either Bool [Expression]

evalOne [] _ (_, Constant _) 		= Left False
evalOne _ [] (_, Constant _) 		= Left False
evalOne [] _ (_, Variable _) 		= Right []
evalOne _ [] (_, Variable _) 		= Right []

evalOne p (c@(e@(s,x), n):cs) y@(q, a@(Constant _))
	| e == y && n == []	= Left True
	| e == y		= Left $ all (== Left True) $ map (evalOne p p) n
	| s == q 		= Left $ all (== Left True) ( map (evalOne p p) (map (\(z,_) -> (z, a)) n) )
	| otherwise		= evalOne p cs y

--evalOne p (c:cs) (q, Variable a) =
