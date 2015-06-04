module Eval3 where

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
	(("q", Constant "d"), []),

	(("k", Constant "d"), []),

	(("r", Variable "X"), [("p", Variable "X"), ("k", Variable "Y"), ("q", Variable "Y")]),

	(("s", Variable "X"), [("q", Variable "X"), ("q", Constant "a")]),

	(("t", Variable "X"), [("p", Variable "X"), ("q", Variable "Y")]),

	(("t", Variable "X"), [("s", Variable "X"), ("q", Variable "Y")])
	]

test :: Expression -> Either Bool [Result]
test x = eval defaultProgram x

eval :: Program -> Expression -> Either Bool [Result]
eval p q@(_, (Variable v))	= either left right result
	where
		result 	= evalOne p p q
		left x	= Left x
		right x	= Right $ filter (\(y,_) -> y == v) x
eval p q = evalOne p p q

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
	| s == q			= Right $ foldl f b bs ++ rest
	| otherwise			= evalOne p cs y
		where
			f l k 		= intersect' l k
			(b:bs)		= rec nx
			nx 			= map (evalOne p p) n
			rec []		= []
			rec (r:rs)	= either (sub1) (sub2) r
				where
					sub1 xy	| xy == True	= rec rs
							| otherwise 	= [[]]
					sub2 xy	= xy:(rec rs)
			Right rest = evalOne p cs y

intersect' :: [Result] -> [Result] -> [Result]
intersect' [] _ 		= []
intersect' _ []			= []
intersect' l r@(rs:_) 	| elem (getX rs) (getAllX l)	= intersectBy (\(g,h) (i,j) -> (g == i && h == j) || g /= i) l r
						| otherwise 					= union l r
	where
		getX (x,_) = x
		getAllX z = map getX z
