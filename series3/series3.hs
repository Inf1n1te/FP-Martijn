import FPPrac.Trees
import Data.Char

---------------------------------------------------------------------------------
-- Example tree with compementary transition functions (because trees in flat
-- text are a real pain!)
--
-- EXAMPLE: showTree $ pp1a $ exampleTree1a exampleTree1
--          showTree $ pp1a $ mapTree (^6) $ exampleTree1a exampleTree1
--
-- USAGE: exampleTree1a exampleTree1
--        exampleTree1b exampleTree1
--        exampleTree1c exampleTree1
--        exampleTree1d exampleTree1


data MultTree v = N v [MultTree v]
                        deriving Show
exampleTree1 :: MultTree (Int,Int)
exampleTree1 = N (50,2) [N (40,5) [N (30,2) [],N (45,2) [N (60,8) [N (61,1) [N (62,3) [N (63,50) []]]]],N (49,2) []],N (56,3) [N (55,2) [],N (57,3) []],N (60,8) [N (61,1) [N (62,3) [N (63,50) []]]],N (40,5) [N (30,2) [],N (45,2) [],N (49,2) []]]

exampleTree1a :: MultTree (Int, Int) -> Tree1a
exampleTree1a (N (x,_) [])       = Leaf1a x
exampleTree1a (N (x,_) [n])      = Node1a x (exampleTree1a n) (Leaf1a 0)
exampleTree1a (N (x,_) (n:o:ns)) = Node1a x (exampleTree1a n) (exampleTree1a o)

exampleTree1b :: MultTree (Int, Int) -> Tree1b
exampleTree1b (N x [])       = Leaf1b x
exampleTree1b (N x [n])      = Node1b x (exampleTree1b n) (Leaf1b (0,0))
exampleTree1b (N x (n:o:ns)) = Node1b x (exampleTree1b n) (exampleTree1b o)


exampleTree4 :: MultTree (Int, Int) -> Tree4
exampleTree4 (N (x,_) [])       = Leaf4
exampleTree4 (N (x,_) [n])      = Node4 x (exampleTree4 n) (Leaf4)
exampleTree4 (N (x,_) (n:o:ns)) = Node4 x (exampleTree4 n) (exampleTree4 o)

exampleTree1d :: MultTree (Int, Int) -> Tree1d
exampleTree1d (N x [])       = Leaf1d x
exampleTree1d (N x [n])      = Node1d [exampleTree1d n]
exampleTree1d (N x ns)     = Node1d (map exampleTree1d ns)

---------------------------------------------------------------------------------

data Tree1a     = Leaf1a Int
                | Node1a Int Tree1a Tree1a
                        deriving Show

pp1a :: Tree1a -> RoseTree
pp1a (Leaf1a n)         = RoseNode (show n) []
pp1a (Node1a i a b)     = RoseNode (show i) [pp1a a, pp1a b]


data Tree1b     = Leaf1b (Int, Int)
                | Node1b (Int, Int) Tree1b Tree1b
                        deriving Show

pp1b :: Tree1b -> RoseTree
pp1b (Leaf1b i)         = RoseNode (show i) []
pp1b (Node1b i a b)     = RoseNode (show i) [pp1b a, pp1b b]


--data Tree1c     = Leaf1c
--                | Node1c Int Tree1c Tree1c
--                        deriving Show
data Tree1c     = Leaf1c Int
                | Node1c Tree1c Tree1c
                    deriving Show

pp1c :: Tree1c -> RoseTree
pp1c (Leaf1c n)           = RoseNode (show n) []
pp1c (Node1c a b)     = RoseNode "" [pp1c a, pp1c b]


data Tree1d     = Leaf1d (Int, Int)
                | Node1d [Tree1d]
                        deriving Show

pp1d :: Tree1d -> RoseTree
pp1d (Leaf1d n) = RoseNode (show n) []
pp1d (Node1d a) = RoseNode "" (map pp1d a)

---ex2
--a

treeAdd :: Int -> Tree1a -> Tree1a
treeAdd i (Leaf1a n)            = Leaf1a (n+i)
treeAdd i (Node1a n a b)        = Node1a (n+i) (treeAdd i a) (treeAdd i b)

--b
treeSquare :: Tree1a -> Tree1a
treeSquare (Leaf1a n)           = Leaf1a (n^2)
treeSquare (Node1a n a b)       = Node1a (n^2) (treeSquare a) (treeSquare b)

--c
mapTree :: (Int -> Int) -> Tree1a -> Tree1a
mapTree f (Leaf1a n)            = Leaf1a (f n)
mapTree f (Node1a n a b)        = Node1a (f n) (mapTree f a) (mapTree f b)

--d
addNode :: Tree1b -> Tree1a
addNode (Leaf1b (x,y))          = Leaf1a (x+y)
addNode (Node1b (x,y) a b)      = Node1a (x+y) (addNode a) (addNode b)

--e
zipWithTree :: (Int -> Int -> Int) -> Tree1b -> Tree1a
zipWithTree f (Leaf1b (x,y))            = Leaf1a (x `f` y)
zipWithTree f (Node1b (x,y) a b)        = Node1a (x `f` y) (zipWithTree f a) (zipWithTree f b)

--new e
mapTreeVar :: ((Int, Int) -> Int) -> Tree1b -> Tree1a
mapTreeVar f (Leaf1b (x,y))     = Leaf1a (f (x, y))
mapTreeVar f (Node1b (x,y) l r) = Node1a (f (x, y)) (mapTreeVar f l) (mapTreeVar f r)

---ex3
--a
binMirror :: Tree1a -> Tree1a
binMirror (Leaf1a n)            = Leaf1a n
binMirror (Node1a n a b )       = Node1a n (binMirror b) (binMirror a)

--b
binMirrorD :: Tree1d -> Tree1d
binMirrorD (Leaf1d (x,y))       = Leaf1d (y,x)
binMirrorD (Node1d a)           = Node1d (map binMirrorD (reverse a))

--Tree 4 -untested!
data Tree4  = Leaf4
            | Node4 Int Tree4 Tree4
                deriving Show

pp4 :: Tree4 -> RoseTree
pp4 (Leaf4)         = RoseNode "" []
pp4 (Node4 n l r)   = RoseNode (show n) [pp4 l, pp4 r]

---ex4
--a
insertTree :: Int -> Tree4 -> Tree4
insertTree n (Leaf4)        = Node4 n Leaf4 Leaf4
insertTree n (Node4 i a b)  | n <= i        = Node4 i (insertTree n a) b
                            | n > i         = Node4 i a (insertTree n b)

--b
makeTree :: [Int] -> Tree4
makeTree []     = Leaf4
makeTree (x:xs) = insertTree x (makeTree xs)

--why no foldl instead of foldr? just curious
makeTreeF :: [Int] -> Tree4
makeTreeF xs    = foldr (insertTree) Leaf4 xs

--c
makeList :: Tree4 -> [Int]
makeList Leaf4         = []
makeList (Node4 i a b) = (makeList a)++[i]++(makeList b)

--d
sortList :: [Int] -> [Int]
sortList xs = makeList $ makeTree xs

--e
sortTree :: Tree4 -> Tree4
sortTree tree = makeTree $ makeList tree

---ex5
subtreeAt :: Int -> Tree4 -> Tree4
subtreeAt i Leaf4           = error "Number not in tree"
subtreeAt i (Node4 n a b)   | i == n        = (Node4 n a b)
                            | i < n         = subtreeAt i a
                            | i > n         = subtreeAt i b

---ex6
{-cutOffAt :: Int -> Tree1c -> Tree1c
cutOffAt _ Leaf1c               = Leaf1c
cutOffAt 0 _                    = Leaf1c
cutOffAt x (Node1c i a b)       = Node1c i (cutOffAt (x-1) a) (cutOffAt (x-1) b)-}
--for a different type of tree -untested!
cutOffAt :: Int -> Tree1a -> Tree1a
cutOffAt _ (Leaf1a n)       = Leaf1a n
cutOffAt 0 (Node1a n _ _)   = Leaf1a n
cutOffAt x (Node1a n l r)   = Node1a n (cutOffAt (x-1) l) (cutOffAt (x-1) r)


---ex7
--a
replace :: Int -> [Char] -> Tree1a -> Tree1a
replace x [] (Leaf1a _)             = Leaf1a x
replace x [] (Node1a _ a b)         = Node1a x a b
replace x _ (Leaf1a i)              = error "Invalid path"
replace x (s:str) (Node1a i a b)    | s == 'l'      = Node1a i (replace x str a) b
                                    | s == 'r'      = Node1a i a (replace x str b)
                                    | otherwise     = error "Invalid character in path"
--b
subTree :: [Char] -> Tree1a -> Tree1a
subTree [] (Leaf1a i)           = Leaf1a i
subTree [] (Node1a i a b)       = Node1a i a b
subTree _ (Leaf1a i)            = error "Invalid path"
subTree (s:str) (Node1a i a b)  | s == 'l'      = subTree str a
                                | s == 'r'      = subTree str b
                                | otherwise     = error "Invalid character in path"

---ex8
--a
isBalanced :: Tree4 -> Bool
isBalanced tree = abs (a - b) < 2
                        where
                                (a, b) = branchMinMax tree

branchMinMax :: Tree4 -> (Int, Int)
branchMinMax (Leaf4)           = (0,0)
branchMinMax (Node4 i a b)     = ( (min (amin+1) (bmin+1)) , (max (amax+1) (bmax+1)) )
                        where
                                (amin, amax) = branchMinMax a
                                (bmin, bmax) = branchMinMax b

isBalancedAlt :: Tree4 -> Bool
isBalancedAlt t = (max - min) < 2
    where 
        xs  = pathLengths t 0
        (max, min) = (maximum xs, minimum xs)

pathLengths :: Tree4 -> Int -> [Int]
pathLengths Leaf4 n = [n]
pathLengths (Node4 _ l r) n = (pathLengths l (n + 1)) ++ (pathLengths r (n + 1)) 

--b
fsthalf :: [Int] -> [Int]
fsthalf xs = take ((length xs) `div` 2) xs
sndhalf :: [Int] -> [Int]
sndhalf xs = drop ((length xs) `div` 2) xs


balance = buildBalancedTree . makeList 

--buildBalancedTree 
buildBalancedTree :: [Int] -> Tree4
buildBalancedTree []    = Leaf4
buildBalancedTree xs    = Node4 (head (sndhalf xs)) (buildBalancedTree (fsthalf xs)) (buildBalancedTree (tail (sndhalf xs)))
