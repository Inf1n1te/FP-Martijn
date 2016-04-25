import Data.Char
import Data.List

-------------------------------------------------
allEqual :: Eq a => [a] -> Bool
allEqual [] 	= True
allEqual [x]	= True
allEqual (x:xs)	| x == head xs	= allEqual xs
		| otherwise	= False
-------------------------------------------------

myfilter :: (a -> Bool) -> [a] -> [a] 
myfilter _ [] 		= []
myfilter f (x:xs)	| f x		= x : myfilter f xs
			| otherwise	= myfilter f xs

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f a [x]		= a `f` x 
myfoldl f a (x:xs)	= myfoldl f (a `f` x) xs 

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f a [x]		= x `f` a 
myfoldr f a xs 		= myfoldr f ((last xs) `f` a) (init xs)


myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

myzipWith _ [] _		= []
myzipWith _ _ []		= []
myzipWith f (x:xs) (y:ys)	= x `f` y : myzipWith f xs ys


----------------------------------
--	   Excercise 2		--
----------------------------------
--a
gba :: [(String,Int,String,String)]
gba = [("a", 15, "M", "Soest"), ("b", 37, "F", "Soest"), ("c", 12, "M", "Soest"), ("d", 25, "F", "Amersfoort")]

getName (a,b,c,d)	= a
getAge (a,b,c,d)	= b
getSex (a,b,c,d)	= c
getCity (a,b,c,d)	= d

--getByName :: String -> (String,Int,String,String)
getByNamePr :: String -> [(String, Int, String, String)] -> (String, Int, String, String)
getByNamePr _ []	= error "not found"
getByNamePr q (x:xs)	| map (toLower) (getName x) == map (toLower) (q)	= x
			| otherwise						= getByNamePr q xs

--B
getByName :: String -> [(String, Int, String, String)] -> (String, Int, String, String)
getByName q xs 	= getByNamePr q xs

getNameByName q xs	= getName (getByName q xs)
getAgeByName q xs	= getAge (getByName q xs)
getSexByName q xs	= getSex (getByName q xs)
getCityByName q xs	= getCity (getByName q xs)


--C
--ageRec :: b -> [(a,b,c,d)] -> [(a,b,c,d)]
ageRec a [] 	= []
ageRec a (x:xs) = (getName x,(getAge x) + a,getSex x ,getCity x) : ageRec a xs

ageList a xs	= [ (getName x,getAge x + a,getSex x ,getCity x) | x <- xs ]

incAge n (a,b,c,d) = (a,b+n,c,d)

ageHOF a xs	= map (incAge a) xs

--D
is30to40F (a,b,c,d) 	| b >= 30 && b <= 40 && c == "F"	= True
			| otherwise				= False

get30to40Rec []		= []
get30to40Rec (x:xs)	| is30to40F x 	= x : get30to40Rec xs
			| otherwise							= get30to40Rec xs

get30to40List xs	= [ (getName x, getAge x, getSex x, getCity x) | x <- xs, is30to40F x ]


get30to40HOF xs		= filter (is30to40F) xs

--E: see B

--F
swapAB (a,b,c,d)	= (b,a,c,d)

sortByAge xs	= map (swapAB) (sort (map (swapAB) xs))

----------------------------------
--	   Excercise 3		--
----------------------------------
--a
sieve []	= []
sieve (x:xs)	= x: sieve (filter (\y-> mod y x /= 0) (xs))

isPrime :: Integer -> Bool

isPrime x	= elem x (sieve [2..x])

firstNPrimes x	= take x (sieve [2..])

primesTo x 	= sieve [2..x]

--b

dividers :: Int -> [Int] 
dividers n	= [ x | x <- [2..n], mod n x == 0 ]

altPrimes n 	= (length (dividers n)) == 1

----------------------------------
--	   Excercise 4		--
----------------------------------
--a
pyth :: (Integral t) => t -> [(t, t, t)]
pyth n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

--b
--pythUniq :: Int -> [(Int, Int, Int)]
pythUniq n = [(x,y,z) |x <- [1..n], y <- [1..n], z <- [1..n], x < y, isPrime z, x^2 + y^2 == z^2]


----------------------------------
--	   Excercise 5		--
----------------------------------
--a
increasing :: (Ord a, Num a) => [a] -> Bool
increasing [x]	= True
increasing (x1:x2:xs) = x1 < x2 && increasing (x2:xs)
--b
weaklyIncreasing :: (Ord a, Num a, Fractional a, Enum a) => [a] -> Bool
weaklyIncreasing [x] 	= True
weaklyIncreasing xs 	= increasing (zipWith (/) (scanl (+) 0 xs) [1..])

---ex6
--a


sublist (x:xs) yss@(y:ys)	| x == y		= allEqual (zipWith (==) xs ys)
				| otherwise		= sublist (x:xs) ys
--TODO fails on [1,4,5,1,2,3] with input [1,2,3]

--b
psublist [] _		= True
psublist _ []		= False
psublist (x:xs) (y:ys)	| x == y	= psublist xs ys
			| otherwise	= psublist (x:xs) ys


---ex7
--a
--bsort xs	= 
--ins :: Ord a => a -> [a] -> [a]

bsort :: Ord a => [a] -> [a]
bsort xs = case bubble xs of
                ys | ys == xs  -> ys
                   | otherwise -> bsort ys
  where bubble (x1:x2:xs) | x1 > x2   = x2:(bubble (x1:xs))
                          | otherwise = x1:(bubble (x2:xs))
        bubble x = x
 
mmsort :: Ord a => [a] -> [a]
mmsort [] = []
mmsort [x] = [x]
mmsort xs = min : (mmsort (xs \\ [min,max])) ++ [max]
    where
        min = minimum xs
        max = maximum xs
       
ins :: Ord a => a -> [a] -> [a]
ins v xs = foldr g (const [v]) xs xs
    where
        g x f xs    | v > x     = x : f (tail xs)
                    | otherwise = v : xs
                   
altIns :: Ord a => a -> [a] -> [a]
altIns v [] = [v]
altIns v xt@(x:xs)  | v <= x    = v:xt
                    | otherwise = x:(altIns v xs)
 
isort :: Ord a => [a] -> [a]
isort = foldr altIns []
 
mysplit :: [a] -> ([a], [a])
mysplit xs = splitAt ((length xs + 1) `div` 2) xs
 
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xt) ys@(y:yt) | x <= y    = x : merge xt ys
                          | otherwise = y : merge xs yt
 
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = let (ys,zs) = mysplit xs
           in merge (msort ys) (msort zs)
 
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort $ filter (<=x) xs) ++ [x] ++ (qsort $ filter (>x) xs)


