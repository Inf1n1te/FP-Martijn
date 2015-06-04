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
test x = eval defaultProgram x
