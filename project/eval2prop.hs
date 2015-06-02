module Eval2prop where

import Data.List

data Argument 	= Variable String
				| Constant String
	deriving (Eq,Show)

type Program = [Clause]
type Clause = (Expression, [Expression])
type Expression = (String, Argument)

defaultProgram :: Program
defaultProgram = [
	(("p", Constant "a"), []),
	(("p", Constant "b"), [("p", Constant "a"), ("p", Constant "c")]),
	(("p", Constant "c"), []),

	(("q", Constant "a"), []),
	(("q", Constant "b"), []),

	(("r", Variable "X"), [("p", Variable "X"), ("q", Variable "X")]),

	(("s", Variable "X"), [("p", Variable "X"), ("q", Constant "a")]),

	(("t", Variable "X"), [("p", Variable "X"), ("q", Variable "Y")])
	]

test :: Expression -> Either Bool [Expression]
test x = evalOne defaultProgram defaultProgram x

evalOne :: Program -> Program -> Expression -> Either Bool [Expression]

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
	| s == q			= Left $ all f $ map (evalOne p p) $ map (\(z, w) -> if w == x then (z, a) else (z, w)) n
	| otherwise			= evalOne p cs y
		where
			f = either (== True) (/= [])

evalOne p ((e@(s, (Constant _)), n):cs) y@(q, (Variable _))
	| s == q && n == []	= Right $ [e] ++ rest
	| s == q 			= if (all (== Left True) ( map (evalOne p p) n)) then (Right $ [e] ++ rest) else (Right $ [] ++ rest)
	| otherwise			= Right rest
		where
			Right rest = evalOne p cs y

evalOne p (c@(e@(s, x@(Variable w)), n@(ns:nss)):cs) y@(q, a@(Variable z))
	| s == q && n == []	= Left True
	| s == q			= Right $ foldl f b bs
		where
			nx 			= map (evalOne p p) n
			(b:bs)		= rec nx
			rec (r:rs)	= either (if (== True) then rec rs else []) (r:(rec rs)) r
			f l k 		= intersect (t l) (t k)
			t			= map (\(i, o) -> (s, o))
