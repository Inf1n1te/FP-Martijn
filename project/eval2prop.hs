module Eval2prop where

import Data.List

data Argument 	= Variable String
				| Constant String
	deriving (Eq,Show)

type Program = [Clause]
type Clause = (Expression, [Expression])
type Expression = (String, Argument)
type Result	= (String, String)

defaultProgram :: Program
defaultProgram = [
	(("p", Constant "a"), []),
	(("p", Constant "b"), [("p", Constant "a"), ("p", Constant "c")]),
	(("p", Constant "c"), []),

	(("q", Constant "a"), []),
	(("q", Constant "c"), []),

	(("r", Variable "X"), [("p", Variable "X"), ("q", Variable "X")]),

	(("s", Variable "X"), [("p", Variable "X"), ("q", Constant "a")]),

	(("t", Variable "X"), [("p", Variable "X"), ("q", Variable "Y")])
	]

test :: Expression -> Either Bool [Result]
test x = evalOne defaultProgram defaultProgram x

evalOne :: Program -> Program -> Expression -> Either Bool [Result]

evalOne [] _ (_, Constant _) 		= Left False
evalOne _ [] (_, Constant _) 		= Left False
evalOne [] _ (_, Variable _) 		= Right []
evalOne _ [] (_, Variable _) 		= Right []

evalOne p ((e@(_, Constant _), n):cs) y@(_, Constant _)
	| e == y && n == []	= Left True
	| e == y			= Left $ all (== Left True) $ map (evalOne p p) n
	| otherwise			= evalOne p cs y

evalOne p (((s, x@(Variable _)), n):cs) y@(q, a@(Constant _))
	| s == q && n == []	= Left True
	| s == q			= Left $ all f $ map (evalOne p p) $ map f' n
	| otherwise			= evalOne p cs y
		where
			f' (z, w)	| w == x 	= (z, a)
						| otherwise = (z, w)
			f 			= either (== True) (/= [])

evalOne p (((s, (Constant x)), n):cs) y@(q, (Variable v))
	| s == q && n == []								= Right $ [(v,x)] ++ rest
	| s == q &&
		(all (== Left True) ( map (evalOne p p) n))	= Right $ [(v,x)] ++ rest
	| s == q 										= Right $ [] ++ rest
	| otherwise			= Right rest
		where
			Right rest = evalOne p cs y

evalOne p (((s, (Variable _)), n):cs) y@(q, (Variable _))
	| s == q && n == []	= Left True
	| s == q			= Right $ foldl f b bs
	| otherwise			= evalOne p cs y
		where
			f l k 		= intersect' (t l) (t k)
			t			= map (\(_, o) -> (s, o))
			(b:bs)		= rec nx
			nx 			= map (evalOne p p) n
			rec []		= []
			rec (r:rs)	= either (sub1) (sub2) r
				where
					sub1 xy	| xy == True	= rec rs
							| otherwise 	= [[]]
					sub2 xy	= xy:(rec rs)

intersect' :: [Result] -> [Result] -> String -> [Result]
intersect' [] _ _						= []
intersect' _ [] _						= []
intersect' l@((w,x):ls) r@((y,z):rs) v	| w == v && y == v = intersectBy (\(_,q) (_,r) -> q == r) l r
