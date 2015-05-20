import FPPrac.Trees
import Data.Char

-- Red-Black-trees in Haskell. 


insert :: Int -> RBTree -> RBTree
insert x tree =	addNode x tree


addNode :: Int -> RBTree -> RBTree
addNode x (RBNode c "" [])	= RBNode c (show x) [(RBNode NodeBlack "" []) , (RBNode NodeBlack "" [])]
addNode x (RBNode c i [l,r])	| x <= (read i)	= RBNode c i [(addNode x l),r] 
				| otherwise	= RBNode c i [l,(addNode x r)]

fixTree :: RBTree -> RBTree

fixTree tree 	| violation == "red" = tree
		| violation == "black"= tree
		| violation == "" = tree
	where violation = checkViolation tree

checkViolation :: RBTree -> String
checkViolation tree 	| snd $violatesRed tree 	= "red"
--			| violatesBlack tree		= "black"
			| otherwise			= ""

violatesRed :: RBTree -> (NodeColor, Bool)
violatesRed (RBNode c _ [])	= (c, False)
violatesRed (RBNode c i [l,r])	= (c, (not noViolation || snd vl || snd vr))
	where 	noViolation = c == NodeBlack || ((fst vl == fst vr) && ((fst vl) == NodeBlack))
		vl = violatesRed l
		vr = violatesRed r

violatesBlack :: RBTree -> (Bool, Int)
violatesBlack (RBNode c _ [])		| c == NodeBlack 	= (False, 1)
					| otherwise		= (True, 0)
violatesBlack (RBNode c _ [l,r])	| 
	where	vl 	=  
