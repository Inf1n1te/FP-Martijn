data Argument 	= Variable String
		| Constant String
	deriving (Eq,Show)

type Program = [Clause]
type Clause = (Expression, [Expression])
type Expression = (String, Argument)

evalOne :: Program -> Program -> Expression -> Either (Bool, [Expressions])
