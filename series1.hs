import Data.Char

ex1 x = 2*x^2 + 3*x - 5

abc = "abcdefghijklmnopqrstuvwxyz"

code :: Char -> Char
code (x)	| ord(x) > 96 && ord(x) < 123 	= chr(97 + mod (ord(x)-97+3) 26)
		| ord(x) > 64 && ord(x) < 91 	= chr(65 + mod (ord(x)-62+3) 26)
		| otherwise 			= x

coden :: Int -> Char -> Char
coden n x	| ord(x) > 96 && ord(x) < 123 	= chr(97 + mod (ord(x)-97+n) 26)
		| ord(x) > 64 && ord(x) < 91 	= chr(65 + mod (ord(x)-65+n) 26)
		| otherwise 			= x

interest :: Floating a => a -> a -> a -> a
interest a r n	= (a*(1+r)**n)

root1 ::  Double -> Double -> Double -> Double
root1 a b c	| disc a b c < 0	= error "negative discriminant"
		| otherwise		= (-b + sqrt(disc a b c)) / (2*a)

root2 ::  Double -> Double -> Double -> Double
root2 a b c	| disc a b c < 0	= error "negative discriminant"
		| otherwise		= (-b - sqrt(disc a b c)) / (2*a)

disc ::  Double -> Double -> Double -> Double
disc a b c	= (b*b)-4*a*c

extrX :: Double -> Double -> Double -> Double
extrX a b c = -b/(2*a)

extrY :: Double -> Double -> Double -> Double
extrY a b c = a*(extrX a b c)^2 + b*(extrX a b c) + c


mylength :: [a] -> Int
mylength [] 	= 0
mylength (x:xs)	= 1 + mylength xs

mysum :: [Int] -> Int
mysum []	= 0
mysum (x:xs)	= x + mysum xs

myreverse :: [a] -> [a]
myreverse []		= []
myreverse (x:xs)	= myreverse xs ++ [x]

mytake :: Int -> [a] -> [a]
mytake n [] 	= []
mytake 0 xs 	= []
mytake n (x:xs) = [x] ++ mytake (n-1) xs

myelem :: Eq a => a -> [a] -> Bool
myelem a []	= False
myelem a (x:xs)	| x == a	= True
		| otherwise	= myelem a xs

myconcat :: [[a]] -> [a]
myconcat [] 	= []
myconcat (x:xs)	= x ++ myconcat xs 

mymaximum :: [Int] -> Int
mymaximum [] 		= 0
mymaximum (x:xs)	| x <= mymaximum(xs) 	= mymaximum(xs)
			| otherwise		= x

myzip :: [a] -> [b] -> [(a, b)]

myzip (x:xs) []		= []
myzip [] (y:ys)		= []
myzip (x:xs) (y:ys)	= (x,y) : myzip xs ys


r :: Num a => a -> a -> [a]
r a d 		= a : r (a+d) d

--r1 :: Num a Eq a => a -> a -> a -> a
r1 a d 0	= a
r1 a d n 	= r1 (a+d) d (n-1)

--total 
--total :: Int a => a -> a -> a -> a -> a

total a d i 0	= 0
total a d 0 j	= mysum (mytake j (r a d))
total a d i j	= total (a+d) d (i-1) (j-1)

allEqual :: Eq a => [a] -> Bool
allEqual [] 	= True
allEqual [x]	= True
allEqual (x:xs)	| x == head xs	= allEqual xs
		| otherwise	= False

matrixSquare :: [[a]] -> Bool
matrixSquare a = allEqual(map mylength a)

rowTotals :: [[Int]] -> [Int]
rowTotals a = map mysum a

transposeMatrix a 	| mylength (head a) == 0	= []	
			| otherwise			= map head a : transposeMatrix (map tail a)

columnTotals a 	| mylength (head a) == 0	= []
		| otherwise			=mysum(map head a) : columnTotals (map tail a)
