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

evalOne :: Program -> Program -> Expression -> Either (Bool, [Expressions])
