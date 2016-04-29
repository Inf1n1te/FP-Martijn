---- Series 4
import FPPrac.Trees
import Data.Char
import Data.List

---------------------------------------------------------------------------------
-- Example tree with compementary transition functions (because trees in flat
-- text are a real pain!)
--
-- EXAMPLE: showTree $ pp1a $ exampleTree1a exampleTree1
--          showTree $ pp1a $ mapTree (^6) $ exampleTree1a exampleTree1

data MultTree x  = Nd x [MultTree x]
                        deriving Show

exampleTree1 :: MultTree (Int,Int)
exampleTree1 = Nd (50,2) [Nd (40,5) [Nd (30,2) [],Nd (45,2) [Nd (60,8) [Nd (61,1) [Nd (62,3) [Nd (63,50) []]]]],Nd (49,2) []],Nd (56,3) [Nd (55,2) [],Nd (57,3) []],Nd (60,8) [Nd (61,1) [Nd (62,3) [Nd (63,50) []]]],Nd (40,5) [Nd (30,2) [],Nd (45,2) [],Nd (49,2) []]]

exampleBinTree :: MultTree (Int, Int) -> BinTree (Int,Int) (Int,Int)
exampleBinTree (Nd x [])           = BinLeaf x
exampleBinTree (Nd x [n])          = BinNode x (exampleBinTree n) (BinLeaf (0,0))
exampleBinTree (Nd x (n:o:ns))     = BinNode x (exampleBinTree n) (exampleBinTree o)

exampleTree2 = BinNode (50,2) (BinNode (40,5) (BinLeaf (30,2)) (BinNode (45,2) (BinNode (61,1) (BinLeaf (62,3)) (BinLeaf ((-1),2))) (BinLeaf (99,3)))) (BinNode (56,3) (BinLeaf (55,2)) (BinLeaf (57,3))) 


---------------------------------------------------------------------------------


---ex1
--a
data BinTree a b        = BinLeaf b
                        | BinNode a (BinTree a b) (BinTree a b)
                                deriving Show
--b
data Unit       = Empty
instance Show Unit where 
        show Empty = ""

type Tree1a     = BinTree Int Int 
type Tree1b     = BinTree (Int, Int) (Int, Int)
type Tree1c     = BinTree Unit Int

--c
pp :: (Show a, Show b) => BinTree a b -> RoseTree
pp (BinLeaf x)          = RoseNode (show x) []
pp (BinNode x a b)      = RoseNode (show x) [pp a, pp b]

---ex2
--BNF:
-- E -> '(' E O E ')'
-- E -> N
-- N -> [0..9]
-- O -> ['+', '-', '/', '^']

isOperand1 :: Char -> Bool
isOperand1 x = elem x "+=/^"

data Expr = E | N | O | V
        deriving Show

parseExpr1 :: Expr -> [Char] -> (BinTree Char Int, [Char])
parseExpr1 E (x:xs)     | x == '('      = (BinNode o t1 t2 ,r3)
                        | isDigit x     = (n, rn)
                        | otherwise     = error $ "parse error at start of expression: "++(x:xs)
                                where
                                        (o, ro)         = parseOpr1 r1
                                        (t1, r1)        = parseExpr1 E xs
                                        (t2, r2)        = parseExpr1 E ro
                                        (n, rn)         = parseExpr1 N (x:xs)
                                        r3              | head r2 == ')' = tail r2
                                                        | otherwise = error $ "parse error on closing bracket before: "++r2
parseExpr1 N (x:xs)     = (BinLeaf $ digitToInt x, xs)
parseOpr1 :: String -> (Char, String) 
parseOpr1 (x:xs)        | isOperand1 x  = (x, xs)
                        | otherwise     = error $ "parse error at start of operand: "++(x:xs)


--BNF:
-- E -> '(' E O E ')'
-- E -> N
-- E -> V
-- N -> [0..9]
-- O -> ['+', '-', '/', '^']
-- V -> [a..zA..z]


parseExpr2 :: Expr -> [Char] -> (BinTree Char (Either Int Char), [Char])
parseExpr2 E (x:xs)     | x == '('      = (BinNode o t1 t2 ,r3)
                        | isDigit x     = (n, rn)
                        | isAlpha x     = (a, ra)
                        | otherwise     = error $ "parse error at start of expression: "++(x:xs)
                                where
                                        (o, ro)         = parseOpr2 r1
                                        (t1, r1)        = parseExpr2 E xs
                                        (t2, r2)        = parseExpr2 E ro
                                        (n, rn)         = parseExpr2 N (x:xs)
                                        (a, ra)         = parseExpr2 V (x:xs)
                                        r3              | head r2 == ')' = tail r2
                                                        | otherwise = error $ "parse error on closing bracket before: "++r2
parseExpr2 N (x:xs)     = (BinLeaf $ Left $ digitToInt x, xs)
parseExpr2 V (x:xs)     = (BinLeaf $ Right x, xs)

parseOpr2 (x:xs)        | isOperand1 x  = (x, xs)
                        | otherwise     = error $ "parse error at start of operand: "++(x:xs)

-- States:

data CalcFsaState = Func | Num | Num2 | Ident | Oper | BraceO | BraceC
        deriving (Show, Eq)

isOperand3 x = elem x "+-/*^=><"
isBrace x = elem x "()"

fsa1 :: CalcFsaState -> Char -> CalcFsaState

fsa1 Func x     | x == '('      = BraceO
                | isDigit x     = Num
                | x == '~'      = Num
                | otherwise     = error "parse error on func"
fsa1 BraceO x   | isDigit x     = Num
                | isAlpha x     = Ident
                | x == '('      = BraceO
                | x == '~'      = Num
                | otherwise     = error "parse error on braceo"
fsa1 BraceC x   | x == ')'      = BraceC
                | isOperand3 x  = Oper
                | otherwise     = error "parse error on bracec"
fsa1 Num x      | isDigit x     = Num
                | isOperand3 x  = Oper
                | x == ')'      = BraceC
                | x == '.'      = Num2
                | otherwise     = error "parse error on num"
fsa1 Num2 x     | isDigit x     = Num2
                | isOperand3 x  = Oper
                | x == ')'      = BraceC
                | otherwise     = error "parse error on num2"
fsa1 Oper x     | isAlpha x     = Ident
                | isDigit x     = Num
                | isOperand3 x  = Oper
                | x == '('      = BraceO
                | x == '~'      = Num
                | otherwise     = error "parse error on oper"
fsa1 Ident x    | isDigit x     = Ident
                | isAlpha x     = Ident
                | isOperand3 x  = Oper
                | x == ')'      = BraceC
                | otherwise     = error "parse error on ident"

testFsa = scanl fsa1 Func "((x123r4^23)+3)"

tokenize :: [Char] -> [(String, CalcFsaState)]
tokenize ys     = concatOnSnd $ zip (map (\x -> [x]) xs) (tail $scanl fsa1 Func xs)
                        where xs = filter (/= ' ') ys

concatOnSnd :: [(String, CalcFsaState)] -> [(String, CalcFsaState)]
concatOnSnd [x] = [x]
concatOnSnd ((x1,y1):(x2,y2):zs)        | (y1 == y2) && (not $elem y1 [BraceO, BraceC]) = concatOnSnd ((x1++x2,y1):zs)
                                        | y1 == Num && y2 == Num2       = concatOnSnd ((x1++x2,y1):zs)
                                        | otherwise                     = (x1,y1) : concatOnSnd ((x2,y2):zs)


--Variant 3
parseExpr3 :: [(String,CalcFsaState)] -> (BinTree String String, [(String, CalcFsaState)] )

parseExpr3 ((str,BraceO):tokens)        = (BinNode op n1 n2, rest)
                where   (n1, r1)        = parseExpr3 tokens
                        (op, r2)        = parseOpr3 r1
                        (n2, r3)        = parseExpr3 r2
                        (br, r4)        = parseBrC3 r3
                        rest            | br == ")"     = r4
                                        | otherwise = error "missing closing brackets"


parseExpr3 ((str,Num):tokens)           = (BinLeaf $ str,tokens)
parseExpr3 ((str,Ident):tokens)         = (BinLeaf $ str,tokens)

parseBrC3 :: [(String,CalcFsaState)] -> (String, [(String, CalcFsaState)] )
parseBrC3 ((str,BraceC):tokens)         = (str,tokens)

parseOpr3 :: [(String,CalcFsaState)] -> (String, [(String, CalcFsaState)] )
parseOpr3 ((str,Oper):tokens)           = (str,tokens)

assign :: ([String], [Double]) -> String -> Double
assign ([],[]) z        = read $ map repl z :: Double
        where   repl '~' = '-'
                repl  x = x
assign (_ ,[]) z        = error "Unequal array lengths"
assign ([],_ ) z        = error "Unequal array lengths"
assign ((x:xs),(y:ys)) z        | z == x        = y
                                | otherwise     = assign (xs,ys) z


-- USAGE:   eval (assign (["x"],[24])) (fst $ parseExpr3 (tokenize "(x+3)"))

eval :: (String -> Double) -> BinTree String String -> Either Double Bool
eval f (BinNode o a1 a2)        | o == "+"      = Left (fromLeft (eval f a1) + fromLeft (eval f a2))
                                | o == "-"      = Left (fromLeft (eval f a1) - fromLeft (eval f a2))
                                | o == "*"      = Left (fromLeft (eval f a1) * fromLeft (eval f a2))
                                | o == "/"      = Left (fromLeft (eval f a1) / fromLeft (eval f a2))
                                | o == "^"      = Left (fromLeft (eval f a1) ** fromLeft (eval f a2))
                                | o == "="      = Right (fromLeft (eval f a1) == fromLeft (eval f a2))
                                | o == "<"      = Right (fromLeft (eval f a1) < fromLeft (eval f a2))
                                | o == ">"      = Right (fromLeft (eval f a1) > fromLeft (eval f a2))
                                | o == "<="     = Right (fromLeft (eval f a1) <= fromLeft (eval f a2))
                                | o == ">="     = Right (fromLeft (eval f a1) >= fromLeft (eval f a2))
                                | o == "/="     = Right (fromLeft (eval f a1) /= fromLeft (eval f a2))
                                | otherwise     = error "operator not recognised"

eval f (BinLeaf x)              = Left (f x)

fromLeft (Left a)               = a
fromLeft (Right _)              = error "Expected Left"
