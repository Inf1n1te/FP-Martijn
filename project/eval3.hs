module Eval3 where

import Data.List

data Argument 	= Variable String
				| Constant String
	deriving (Eq,Show)

type Program = [Clause]
type Clause = (Expression, [Expression])
type Expression = (String, [Argument])
type Result	= (String, String)

defaultProgram :: Program
defaultProgram = [
	(("p", [Constant "a"]), []),
	(("p", [Constant "b"]), [("p", [Constant "a"]), ("p", [Constant "c"])]),
	(("p", [Constant "c"]), []),

	(("q", [Constant "a"]), []),
	(("q", [Constant "d"]), []),

	(("k", [Constant "d"]), []),

	(("r", [Variable "X"]), [("p", [Variable "X"]), ("k", [Variable "Y"]), ("q", [Variable "Y"])]),

	(("s", [Variable "X"]), [("q", [Variable "X"]), ("q", [Constant "a"])]),

	(("t", [Variable "X"]), [("p", [Variable "X"]), ("q", [Variable "Y"])]),

	(("t", [Variable "X"]), [("s", [Variable "X"]), ("q", [Variable "Y"])])
	]

test :: Expression -> Either Bool [Result]
test x = evalMulti defaultProgram defaultProgram x

evalMulti :: Program -> Program -> Expression -> Either Bool [Result]
evalMulti p (c@(e@(e_str, e_agrs@(e_arg:e_arg_tail)), exprs):cs) q@(q_str, q_args@(q_arg:q_arg_tail))
	|

intersect'' :: [Result] -> [Result] -> [Result]
intersect'' l r = foldl intersect' l (s s')
	where
		s'			  = sort r
		s []			= []
		s l@((x,y):_)	= [b] ++ s (l \\ b)
			where
				b 		= takeWhile (\(i,_) -> i == x) l

intersect' :: [Result] -> [Result] -> [Result]
intersect' [] _ 		= []
intersect' _ []			= []
intersect' l s@(r:_) 	| elem (getX r) (getAllX l)	= intersectBy (\(g,h) (i,j) -> (g == i && h == j) || g /= i) l s
						| otherwise 					= union l s
	where
		getX (x,_)	  = x
		getAllX z	   = map getX z
