module Eval3 where

import Data.List
import Data.Either

data Argument 	= Variable String
				| Constant String
	deriving (Eq,Show)

type Program = [Clause]
type Clause = (Expression, [Expression])
type Expression = (String, [Argument])
type Result	= Either Bool [(String, String)]

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

oldDefaultProgram2 :: Program
oldDefaultProgram2 = [
	(("p", []), []),
	(("p", [Constant "a", Variable "X"]), []),
	(("p", [Variable "X", Constant "b"]), []),
	(("p", [Variable "X", Variable "Y"]), [])
	]

defaultProgram :: Program
defaultProgram = [
	(("w", [Constant "j"]),[]),
	(("w", [Constant "b"]),[]),
	(("w", [Constant "m"]),[]),
	(("w", [Constant "i"]),[]),
	(("w", [Constant "c"]),[]),
	(("m", [Constant "be"]),[]),
	(("m", [Constant "w"]),[]),
	(("mother", [Constant "j", Constant "b" ]),[]),
	(("mother", [Constant "b", Constant "w" ]),[]),
	(("mother", [Constant "j", Constant "m" ]),[]),
	(("mother", [Constant "j", Constant "i" ]),[]),
	(("father", [Constant "be", Constant "b" ]),[]),
	(("father", [Constant "be", Constant "m" ]),[]),
	(("child", [Variable "K", Variable "O"]), [("mother", [Variable "O", Variable "K"])]),
	(("child", [Variable "K", Variable "O"]), [("father", [Variable "O", Variable "K"])]),
	(("son", [Variable "Z", Variable "O"]) ,[("child", [Variable "Z", Variable "O"]), ("m", [Variable "Z"])]),
	(("daughter", [Variable "Z", Variable "O"]) ,[("child", [Variable "Z", Variable "O"]), ("w", [Variable "Z"])]),
	(("sister", [Variable "X", Variable "Y"]), [("child", [Variable "X", Variable "O"]), ("w", [Variable "X"]), ("child", [Variable "Y", Variable "O"])]),
	(("motherfather", [Variable "X", Variable "Y", Variable "Z"]), [("child", [Variable "Y", Variable "X"]), ("child", [Variable "Y", Variable "Z"])]),
	(("brother", [Variable "X", Variable "Y"]), [("child", [Variable "X", Variable "O"]), ("m", [Variable "X"]), ("child", [Variable "Y", Variable "O"])])
	]

test :: Expression -> [Result]
test x = eval defaultProgram x

eval :: Program -> Expression -> [Result]
eval p q@(_, q_args) 	| null (evalMulti p p q)					= [Left False]
						| all (== (Left True)) (evalMulti p p q)	= [Left True]
						| otherwise 								= filter f (evalMulti p p q)
	where
		f 							= either left right
		left x						= x
		right (x,_)					= elem x $ getArgs q_args
		getArgs []					= []
		getArgs ((Variable x):xs)	= x:(getArgs xs)
		getArgs ((Constant x):xs)	= getArgs xs

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
					x 	= map simpleSubstitute $ zip e_args q_args
						where
							simpleSubstitute (Constant z, Constant y)	= Left $ z == y
							simpleSubstitute (Constant z, Variable y) = Right (y, z)
							simpleSubstitute _ = Left True

evalMulti p (c@(e@(e_str, e_args@(e_arg:e_args_tail)), es):cs) q@(q_str, q_args@(q_arg:q_args_tail))
	| e_str == q_str && length e_args == length q_args && (null $ check e_args q_args [Right ("remove_after","remove_after")])
		= result ++ (evalMulti p cs q)
	| e_str == q_str && length e_args == length q_args
		= intersect''' ((check e_args q_args [Right ("remove_after","remove_after")]) \\ [Right ("remove_after","remove_after")]) result ++ (evalMulti p cs q)
	| otherwise
		= evalMulti p cs q
			where
				result 					= foldl intersect''' b bs
				(b:bs) 					= map (eval p) $ substitute rq_args re_args res
					where
						(rq_args, re_args, res) = refactor q_args e_args es



intersect''' :: [Result] -> [Result] -> [Result]
intersect''' l r
	| elem (Left False) (l ++ r)	= []
	| otherwise						= map (\x -> Right x) (intersect' lr rr) -- ++ ll ++ rl
		where
			lr = makeSingle $ rights l
			ll = map (\x -> Left x) $ lefts l
			rr = rights r
			rl = map (\x -> Left x) $ lefts r

-- intersect'' :: [[(String, String)]] -> [[(String, String)]] -> [[(String, String)]]
-- intersect'' l r = foldl intersect' l (s s')
-- 	where
-- 		s'			  	= sort r
-- 		s []			= []
-- 		s l@((x,y):_)	= [b] ++ s (l \\ b)
-- 			where
-- 				b 		= takeWhile (\(i,_) -> i == x) l

intersect' :: [[(String, String)]] -> [[(String, String)]] -> [[(String, String)]]
intersect' [] _ 		= []
intersect' _ []			= []
intersect' lss@(l:ls) rss@(r:rs) = -- \\TODO check and intersect
-- intersect' l s@(r:_) 	| elem (getX r) (getAllX l)	= intersectBy (\(g,h) (i,j) -> (g == i && h == j) || g /= i) l s
-- 						| otherwise 					= union l s
-- 	where
-- 		getX (x,_)	  = x
-- 		getAllX z	  = map getX z

makeSingle :: [[(String,String)]] -> [[(String,String)]]
makeSingle []			= []
makeSingle ([x]:xs)		= [[x]] ++ (makeSingle xs)
makeSingle ((x:y):xs)	= [[x]] ++ (makeSingle ([y]++xs))

refactor :: [Argument] -> [Argument] -> [Expression] -> ([Argument], [Argument], [Expression])
refactor qa ea es'
	= (qa, (subConflictea), (subConflictes'))
		where
			subConflictea = snd $ head $ substitute (map argSub qa) qa [("bla",ea)]
			argSub (Variable qai) = (Variable (qai++"arg"))
			argSub (Constant qai) = (Constant (qai++"arg"))
			subConflictes' = substitute (map argSub qa) qa es'

substitute :: [Argument] -> [Argument] -> [Expression] -> [Expression]
substitute [] [] exs	=  exs
substitute (q1:q_rest) (e1:e_rest) exs
	= substitute q_rest e_rest $ substitution q1 e1 exs
check :: [Argument] -> [Argument] -> [Result] -> [Result]
check [] [] _			= []
check ((Constant x):e_args_tail) ((Constant y):q_args_tail) extras
	| x == y 			= check e_args_tail q_args_tail extras
	| otherwise			= []
check ((Constant x):e_args_tail) ((Variable y):q_args_tail) extras
	= union [Right (y, x)] extras
check (_:e_args_tail) (_:q_args_tail) extras
	= check e_args_tail q_args_tail extras
substitution :: Argument -> Argument -> [Expression] -> [Expression]
substitution qq@(Variable q_a) ee@(Variable e_a) exs	= map (\(z,y) -> (z, map (replace) y)) exs
	where
		replace x 	| x == ee	= qq
					| otherwise = x
substitution qq@(Constant q_a) ee@(Variable e_a) exs	= map (\(z,y) -> (z, map (replace) y)) exs
	where
		replace x 	| x == ee	= qq
					| otherwise = x
substitution qq ee exs 									= exs
