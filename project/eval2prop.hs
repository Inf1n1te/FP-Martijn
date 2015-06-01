data Argument 	= Variable String
		| Constant String
	deriving (Eq,Show)

type Program = [Clause]
type Clause = (Expression, [Expression])
type Expression = (String, Argument)

evalOne :: Program -> Program -> Expression -> (Either Bool [Expression])

evalOne [] _ (_, Constant _) 		= Left False
evalOne _ [] (_, Constant _) 		= Left False
evalOne [] _ (_, Variable _) 		= Right []
evalOne _ [] (_, Variable _) 		= Right []

evalOne p (c@(e@(s,x), n):cs) y@(q, Constant a) 
	| e == y && n == []	= Left True
	| e == y		= all (\(Left x) -> x == True) $ map (evalOne p p) n
	| s == q 		= all (\(Left x) -> x == True) ( map (evalOne p p) (map (\(x,_) -> (x,a)) n) )
	| otherwise		= evalOne p cs y 

--evalOne p (c:cs) (q, Variable a) = 
