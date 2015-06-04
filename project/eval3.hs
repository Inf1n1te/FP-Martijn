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
evalMulti [] _ _ 	= error "empty program"
evalMulti _ [] _ 	= Right []
evalMulti p (c@(e@(e_str, e_args@(e_arg:e_args_tail)), es):cs) q@(q_str, q_args@(q_arg:q_args_tail))
	| e_str == q_str && null es && length e_args == length q_args && (all (== True) $ map no_es_check $ zip e_args q_args)
		= Left True
	| e_str == q_str && null es && length e_args == length q_args
		= evalMulti p cs q
	| e_str == q_str && null es
		= Left False
	| e_str == q_str
		= Right []
	| otherwise
		= evalMulti p cs q
		where
			no_es_check	(Constant x, Constant y)	= x == y
			no_es_check	_							= True

intersect'' :: [Result] -> [Result] -> [Result]
intersect'' l r = foldl intersect' l (s s')
	where
		s'			  	= sort r
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
		getAllX z	  = map getX z
