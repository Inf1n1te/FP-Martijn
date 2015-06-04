module Eval3 where

import Data.List
import Data.Either

data Argument 	= Variable String
				| Constant String
	deriving (Eq,Show)

type Program = [Clause]
type Clause = (Expression, [Expression])
type Expression = (String, [Argument])
type Result	= Either Bool (String, String)

oldDefaultProgram :: Program
oldDefaultProgram = [
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

defaultProgram :: Program
defaultProgram = [
	(("p", []), []),
	(("p", [Constant "a", Variable "X"]), []),
	(("p", [Variable "X", Constant "b"]), []),
	(("p", [Variable "X", Variable "Y"]), [])
	]

test :: Expression -> [Result]
test x = evalMulti defaultProgram defaultProgram x

evalMulti :: Program -> Program -> Expression -> [Result]

evalMulti [] _ _ 	= error "empty program"

evalMulti _ [] _ 	= []

evalMulti p (c@(e@(e_str, e_args), []):cs) q@(q_str, q_args)
	| e_str == q_str && length e_args == length q_args
		= process ++ (evalMulti p cs q)
	| otherwise
		= evalMulti p cs q
		where
			process :: [Result]
			process		| elem (Left False) x	= []
						| all (== Left True) x 	= [Left True]
						| otherwise 			= map (\y -> Right y) (rights x)
				where
					x 	= map substitute $ zip e_args q_args
						where
							substitute (Constant z, Constant y)	= Left $ z == y
							substitute (Constant z, Variable y) = Right (y, z)
							substitute _ = Left True

evalMulti p (c@(e@(e_str, e_args@(e_arg:e_args_tail)), es):cs) q@(q_str, q_args@(q_arg:q_args_tail))
	| e_str == q_str && length e_args == length q_args && (null $ check e_args q_args [("remove_after","remove_after")])
		= []
	| e_str == q_str && length e_args == length q_args
		= ((check e_args q_args [("remove_after","remove_after")]) \\ [("remove_after","remove_after")])
			where
				check [] [] _	= []
				check ((_, Constant x):e_args_tail) ((_, Constant y):q_args_tail) _
					| x == y 	= check e_args_tail q_args_tail
					| otherwise	= []
				check ((_, Constant x):e_args_tail) ((_, Variable y):q_args_tail) extras
					= union [Right (y, x)] extras
				substitute (\\TODO_ q_a) (Variable e_a) exs	= map (map (replace) exs)
					where
						replace x 	| x == e_a	= q_a
									| otherwise = x

	| otherwise
		= evalMulti p cs q

intersect''' :: [Result] -> [Result] -> [Result]
intersect''' l r
	| elem (Left False) (l ++ r)	= []
	| otherwise						= map (\x -> Right x) (intersect'' lr rr) ++ ll ++ rl
		where
			lr = rights l
			ll = map (\x -> Left x) $ lefts l
			rr = rights r
			rl = map (\x -> Left x) $ lefts r

intersect'' :: [(String, String)] -> [(String, String)] -> [(String, String)]
intersect'' l r = foldl intersect' l (s s')
	where
		s'			  	= sort r
		s []			= []
		s l@((x,y):_)	= [b] ++ s (l \\ b)
			where
				b 		= takeWhile (\(i,_) -> i == x) l

intersect' :: [(String, String)] -> [(String, String)] -> [(String, String)]
intersect' [] _ 		= []
intersect' _ []			= []
intersect' l s@(r:_) 	| elem (getX r) (getAllX l)	= intersectBy (\(g,h) (i,j) -> (g == i && h == j) || g /= i) l s
						| otherwise 					= union l s
	where
		getX (x,_)	  = x
		getAllX z	  = map getX z
