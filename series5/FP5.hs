module FP5 where
import Data.Char
import Data.List
import FPPrac.Trees
import Data.Maybe

data Colour	= Red
			| Black
			| Grey
	deriving (Eq, Show)
data Tree 	= Node Colour (Maybe (Int, Tree, Tree))
	deriving (Eq, Show)

pp :: Tree -> RBTree
pp (Node c Nothing) = RBNode n "" []
	where
		n 	| c == Red 		= NodeRed
			| c == Black 	= NodeBlack
			| c == Grey 	= NodeGrey
pp (Node c (Just (v, l, r))) = RBNode n s [pp l, pp r]
	where
		s = show v
		n 	| c == Red 		= NodeRed
			| c == Black 	= NodeBlack
			| c == Grey 	= NodeGrey
			
blackLeaf :: Tree
blackLeaf = Node Black Nothing

greyLeaf :: Tree
greyLeaf = Node Grey Nothing

showTree :: [Int] -> IO ()
showTree xs = showRBTree $ pp $ balancedListInsertion blackLeaf xs
{-
-------------------------------------------------------------------------------
--------------------------------INSERTION--------------------------------------
-------------------------------------------------------------------------------
-}
balancedListInsertion :: Tree -> [Int] -> Tree
balancedListInsertion t [] = t
balancedListInsertion t (x:xs) = balancedInsertion (balancedListInsertion t xs) x

balancedInsertion :: Tree -> Int -> Tree
balancedInsertion t x = rootToBlack $ fixRed $ insertion t x

insertion :: Tree -> Int -> Tree
insertion (Node _ Nothing) x = Node Red (Just (x, blackLeaf, blackLeaf))
insertion (Node c (Just(v, l, r))) x	| x > v 	= Node c (Just (v, l, (insertion r x)))
										| otherwise	= Node c (Just (v, (insertion l x), r))
							
rootToBlack :: Tree -> Tree
rootToBlack (Node c (Just (v, l, r))) = Node Black (Just (v, l, r))

repairRed :: Tree -> Tree
-- colourFlip
repairRed (Node Black (Just (x, (Node Red (Just (y, ll@(Node Red (Just (w, lll, llr))), lr))), (Node Red (Just (z, rl, rr)))))) = Node Red (Just (x, (Node Black (Just (y, ll, lr))), (Node Black (Just (z, rl, rr)))))
repairRed (Node Black (Just (x, (Node Red (Just (y, ll, lr@(Node Red (Just (w, lll, llr)))))), (Node Red (Just (z, rl, rr)))))) = Node Red (Just (x, (Node Black (Just (y, ll, lr))), (Node Black (Just (z, rl, rr)))))
repairRed (Node Black (Just (x, (Node Red (Just (y, ll, lr))), (Node Red (Just (z, rl@(Node Red (Just (w, lll, llr))), rr)))))) = Node Red (Just (x, (Node Black (Just (y, ll, lr))), (Node Black (Just (z, rl, rr)))))
repairRed (Node Black (Just (x, (Node Red (Just (y, ll, lr))), (Node Red (Just (z, rl, rr@(Node Red (Just (w, lll, llr))))))))) = Node Red (Just (x, (Node Black (Just (y, ll, lr))), (Node Black (Just (z, rl, rr)))))
-- rebalance
repairRed (Node Black (Just (x, (Node Red (Just (y, (Node Red (Just (z, lll, llr))), lr))), r))) = Node Black (Just (y, (Node Red (Just (z, lll, llr))), (Node Red (Just (x, lr, r)))))
repairRed (Node Black (Just (x, l, (Node Red (Just (y, rl, (Node Red (Just (z, rrl, rrr))))))))) = Node Black (Just (y, (Node Red (Just (x, l, rl))), (Node Red (Just (z, rrl, rrr)))))
repairRed (Node Black (Just (x, (Node Red (Just (y, ll, (Node Red (Just (z, lrl, lrr)))))), r))) = Node Black (Just (z, (Node Red (Just (y, ll, lrl))), (Node Red (Just (x, lrr, r)))))
repairRed (Node Black (Just (x, l, (Node Red (Just (y, (Node Red (Just (z, rll, rlr))), rr)))))) = Node Black (Just (z, (Node Red (Just (x, l, rll))), (Node Red (Just (y, rlr, rr)))))
-- otherwise
repairRed t = t


fixRed :: Tree -> Tree
fixRed t@(Node _ Nothing) = t
fixRed t@(Node a (Just (b, t1, t2))) = repairRed(Node a (Just (b, f1, f2)))
	where
		f1 = fixRed t1
		f2 = fixRed t2
		
{-
-------------------------------------------------------------------------------
---------------------------------DELETION--------------------------------------
-------------------------------------------------------------------------------
-}

leftMostValue :: Tree -> Tree
leftMostValue (Node _ Nothing) = error "LeafException: Must use function on Node"
leftMostValue n@(Node _ (Just (_, (Node _ Nothing),_))) = n
leftMostValue (Node _ ( Just (_, n@(Node _ (Just (_, _, _))),_))) = leftMostValue n

removeLeftMostNode :: Tree -> Tree
removeLeftMostNode (Node Black (Just (_, (Node _ Nothing), (Node _ Nothing)))) = greyLeaf
removeLeftMostNode (Node Red (Just (_, (Node _ Nothing), (Node _ Nothing)))) = blackLeaf
removeLeftMostNode (Node c (Just (_, (Node _ Nothing), Node _ (Just (v, l, r))))) = Node c (Just (v, l, r))
removeLeftMostNode (Node c (Just (v, l, r))) = Node c (Just (v, (removeLeftMostNode l), r))

greyColourFlip :: Tree -> Tree
--	Pattern 1
--		Normal
greyColourFlip (Node Black (Just (p, (Node Grey g), (Node Black (Just (s, sl@(Node Black l), sr@(Node Black r)))))))	= Node Grey (Just (p, (Node Black g), (Node Red (Just (s, sl, sr)))))
--		Mirrored
greyColourFlip (Node Black (Just (p, (Node Black (Just (s, sl@(Node Black l), sr@(Node Black r)))), (Node Grey g))))	= Node Grey (Just (p, (Node Red (Just (s, sl, sr))), (Node Black g)))
-- 	Pattern 2
--		Normal
greyColourFlip (Node cp (Just (p, (Node Grey g), (Node Black (Just (s, (Node Red (Just (l, (Node Black a), (Node Black b)))), r)))))) 	= Node cp (Just (l, (Node Black (Just (p, (Node Black g), (Node Black a)))), (Node Black (Just (s, (Node Black b), r)))))
--		Mirrored
greyColourFlip (Node cp (Just (p, (Node Black (Just (s, r, (Node Red (Just (l, (Node Black b), (Node Black a))))))), (Node Grey g)))) 	= Node cp (Just (l, (Node Black (Just (s, r, (Node Black b)))), (Node Black (Just (p, (Node Black a), (Node Black g))))))
-- 	Pattern 3
--		Normal
greyColourFlip (Node Red (Just (p, (Node Grey g), (Node Black (Just (s, (Node Black l), r))))))	= Node Black (Just (s, (Node Red (Just (p, (Node Black g), (Node Black l)))), r))
--		Mirrored
greyColourFlip (Node Red (Just (p, (Node Black (Just (s, r, (Node Black l)))), (Node Grey g))))	= Node Black (Just (s, r, (Node Red (Just (p, (Node Black l), (Node Black g))))))
-- 	Pattern 4
--		Normal
greyColourFlip (Node Black (Just (p, (Node Grey g), (Node Black (Just (s, (Node Black l), (Node Red r)))))))	= Node Black (Just (s, (Node Black (Just (p, (Node Black g), (Node Black l)))), (Node Black r)))
--		Mirrored
greyColourFlip (Node Black (Just (p, (Node Black (Just (s, (Node Red r), (Node Black l)))), (Node Grey g))))	= Node Black (Just (s, (Node Black r), (Node Black (Just (p, (Node Black l), (Node Black g))))))
--	Pattern 5
-- 		Normal
greyColourFlip (Node Black (Just (p, (Node Grey g), (Node Red (Just (s, (Node Black l), (Node Black r)))))))	= Node Black (Just (s, greyColourFlip (Node Red (Just (p, (Node Grey g), (Node Black l)))), (Node Black r)))
-- 		Mirrored
greyColourFlip (Node Black (Just (p, (Node Red (Just (s, (Node Black r), (Node Black l)))), (Node Grey g))))	= Node Black (Just (s, (Node Black r), greyColourFlip (Node Red (Just (p, (Node Black l), (Node Grey g))))))
greyColourFlip t = t

greyRebalance :: Tree -> Tree

--fixRed t@(Node a (Just (b, t1, t2))) = repairRed(Node a (Just (b, f1, f2)))

greyRebalance t@(Node _ Nothing) = t
greyRebalance t@(Node a (Just (b, t1, t2))) = greyColourFlip (Node a (Just (b, f1, f2)))
	where 	f1 = greyRebalance t1
		f2 = greyRebalance t2

balancedDelete :: Tree -> Int -> Tree
balancedDelete t n = rootToBlack $ greyRebalance $ deleteNode t n

deleteValue :: Tree -> Tree
deleteValue (Node c (Just (n, (Node cl (Just (vl, ll, lr))), (Node _ Nothing)))) = Node c (Just (vl, ll, lr))
deleteValue (Node c (Just (n, l@(Node cl (Just (vl, ll, lr))), r@(Node cr (Just (vr,rl,rr)))))) = Node c (Just (vlm, l, (removeLeftMostNode r)))
	where (Node clm (Just (vlm, _, _))) = leftMostValue r
deleteValue t = removeLeftMostNode t
		


deleteNode :: Tree -> Int -> Tree
deleteNode (Node _ Nothing) n = error "Node not found"
deleteNode t@(Node c (Just (tv, t1, t2))) n	| tv < n	= Node c (Just (tv, t1, (deleteNode t2 n)))
						| tv > n	= Node c (Just (tv, (deleteNode t1 n), t2))
						| otherwise	= deleteValue t




{- OLD
	--	Pattern 1
	--		Normal
	greyColourFlip (Node Black p (Node Grey g gl gr) (Node Black s (Node Black l ll lr) (Node Black r rl rr))) 	= Node Grey p (Node Black g gl gr) (Node Red s (Node Black l ll lr) (Node Black r rl rr))
	greyColourFlip (Node Black p (Node Grey g gl gr) (Node Black s (Node Black l ll lr) (Leaf Black))) 			= Node Grey p (Node Black g gl gr) (Node Red s (Node Black l ll lr) (blackLeaf))
	greyColourFlip (Node Black p (Node Grey g gl gr) (Node Black s (Leaf Black) (Node Black r rl rr))) 			= Node Grey p (Node Black g gl gr) (Node Red s (blackLeaf) (Node Black r rl rr))
	greyColourFlip (Node Black p (Node Grey g gl gr) (Node Black s (Leaf Black) (Leaf Black))) 					= Node Grey p (Node Black g gl gr) (Node Red s (blackLeaf) (blackLeaf))
	greyColourFlip (Node Black p (Leaf Grey) (Node Black s (Node Black l ll lr) (Node Black r rl rr))) 			= Node Grey p (blackLeaf) (Node Red s (Node Black l ll lr) (Node Black r rl rr))
	greyColourFlip (Node Black p (Leaf Grey) (Node Black s (Node Black l ll lr) (Leaf Black))) 					= Node Grey p (blackLeaf) (Node Red s (Node Black l ll lr) (blackLeaf))
	greyColourFlip (Node Black p (Leaf Grey) (Node Black s (Leaf Black) (Node Black r rl rr))) 					= Node Grey p (blackLeaf) (Node Red s (blackLeaf) (Node Black r rl rr))
	greyColourFlip (Node Black p (Leaf Grey) (Node Black s (Leaf Black) (Leaf Black))) 							= Node Grey p (blackLeaf) (Node Red s (greyLeaf) (greyLeaf))
	--		Mirrored
	greyColourFlip (Node Black p (Node Black s (Node Black l ll lr) (Node Black r rl rr)) (Node Grey g gl gr)) 	= Node Grey p (Node Red s (Node Black l ll lr) (Node Black r rl rr)) (Node Black g gl gr)
	greyColourFlip (Node Black p (Node Black s (Node Black l ll lr) (Leaf Black)) (Node Grey g gl gr)) 			= Node Grey p (Node Red s (Node Black l ll lr) (blackLeaf)) (Node Black g gl gr)
	greyColourFlip (Node Black p (Node Black s (Leaf Black) (Node Black r rl rr)) (Node Grey g gl gr)) 			= Node Grey p (Node Red s (blackLeaf) (Node Black r rl rr)) (Node Black g gl gr)
	greyColourFlip (Node Black p (Node Black s (Leaf Black) (Leaf Black)) (Node Grey g gl gr)) 					= Node Grey p (Node Red s (blackLeaf) (blackLeaf)) (Node Black g gl gr)
	greyColourFlip (Node Black p (Node Black s (Node Black l ll lr) (Node Black r rl rr)) (Leaf Grey)) 			= Node Grey p (Node Red s (Node Black l ll lr) (Node Black r rl rr)) (blackLeaf)
	greyColourFlip (Node Black p (Node Black s (Node Black l ll lr) (Leaf Black)) (Leaf Grey)) 					= Node Grey p (Node Red s (Node Black l ll lr) (blackLeaf)) (blackLeaf)
	greyColourFlip (Node Black p (Node Black s (Leaf Black) (Node Black r rl rr)) (Leaf Grey)) 					= Node Grey p (Node Red s (blackLeaf) (Node Black r rl rr)) (blackLeaf)
	greyColourFlip (Node Black p (Node Black s (Leaf Black) (Leaf Black)) (Leaf Grey)) 							= Node Grey p (Node Red s (greyLeaf) (greyLeaf)) (blackLeaf)
	--	Pattern 2
	--		Normal
	greyColourFlip (Node cp p (Node Grey g gl gr) (Node Black s (Node Red l (Node Black a al ar) (Node Black b bl br)) r)) 	= Node cp l (Node Black p (Node Black g gl gr) (Node Black a al ar)) (Node Black s (Node Black b bl br) r)
-}
