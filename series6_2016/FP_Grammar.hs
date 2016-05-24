{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}
        -- Necessary for function toRoseTree

module FP_Grammar where

{- ===========================================================================
Contains example grammar + examples of test definitions
NOTE: Compiler directives above
=========================================================================== -}

import FPPrac.Trees       -- Contains now also the function toRoseTree. Re-install it!
import GHC.Generics       -- Necessary for correct function of FPPrac

import FP_TypesEtc           -- Extend the file TypesEtc with your own alphabet
import FP_ParserGen (parse)  -- Touching this file leaves you at your own devices
-- import Tokenizer       -- You'll have to write a file for tokenizing yourself

-- ==========================================================================================================
-- Example grammar, to illustrate the structure of the definition of the grammar as a function
--      (where the Alphabet is in the file TypesEtc.hs)

grammar :: Grammar

grammar nt = case nt of

        Nmbr    -> [[ nmbr                               ]]

        Op      -> [[ op                                 ]]

        Var     -> [[ var                                ]]

        Expr    -> [[ lBracket, Expr, Op, Expr, rBracket ]
                   ,[ Nmbr                               ]
                   ,[ Var                                   ]]
        Stat    -> [[ Var, asm, Expr                        ]
                   ,[ rep, Expr, Block                      ]
                   ,[ ifstr, Expr, thenstr, Block, Opt [elsestr, Block]]
                   ,[ Expr                                  ]]
        Block   -> [[ lBrace, (+:) [Stat], rBrace           ]]
        Program -> [[ Block                                 ]]


-- shorthand names can be handy, such as:
lBracket  = Terminal "("           -- Terminals WILL be shown in the parse tree
rBracket  = Terminal ")"
lBrace    = Terminal  "{"
rBrace    = Terminal  "}"
asm       = Terminal ":="
rep       = Terminal "repeat"
ifstr     = Terminal "if"
thenstr   = Terminal "then"
elsestr   = Terminal "else"

-- alternative:
-- lBracket  = Symbol "("          -- Symbols will NOT be shown in the parse tree.
-- rBracket  = Symbol ")"

nmbr        = SyntCat Nmbr
op          = SyntCat Op
var         = SyntCat Var

-- ==========================================================================================================
-- TESTING: example expression: "((10+20)*30)"

-- Result of tokenizer (to write yourself) should be something like:
tokenList0 = [ (Bracket,"(",0)
             , (Bracket,"(",1)
             , (Nmbr,"10",2)
             , (Op,"+",3)
             , (Nmbr,"20",4)
             , (Bracket,")",5)
             , (Op,"*",6)
             , (Nmbr,"30",7)
             , (Bracket,")",8)
             ]

tokenList1 :: [Token]             
tokenList1 = [ (Brace,      "{",        0)
             , (Rep,        "repeat",   1)
             , (Bracket,    "(",        2)
             , (Nmbr,       "10",       3)
             , (Op,         "+",        4)
             , (Nmbr,       "20",       5)
             , (Bracket,    ")",        6)
             , (Brace,      "{",        7)
             , (Var,        "a",        8)
             , (Asm,        ":=",       9)
             , (Bracket,    "(",        10)
             , (Nmbr,       "3.5",      11)
             , (Op,         "+",        12)
             , (Nmbr,       "4",        13)
             , (Bracket,    ")",        14)
             , (Brace,      "}",        15)        
             , (Var,        "b",        16)
             , (Asm,        ":=",       17)
             , (Bracket,    "(",        18)
             , (Nmbr,       "2.5",      19)
             , (Op,         "+",        20)
             , (Nmbr,       "8",        21)
             , (Bracket,    ")",        22)
             , (Brace,      "}",        23)
             ]
             
tokenList2 = [ (Brace,      "{")
             , (Rep,        "repeat")
             , (Bracket,    "(")
             , (Nmbr,       "10")
             , (Op,         "+")
             , (Nmbr,       "20")
             , (Bracket,    ")")
             , (Brace,      "{")
             , (Var,        "a")
             , (Asm,        ":=")
             , (Bracket,    "(")
             , (Nmbr,       "3.5")
             , (Op,         "+")
             , (Nmbr,       "4")
             , (Bracket,    ")")
             , (Brace,      "}") 
             , (If,         "if") 
             , (Bracket,    "(") 
             , (Nmbr,       "3") 
             , (Op,         "<") 
             , (Var,        "a") 
             , (Bracket,    ")") 
             , (Then,       "then") 
             , (Brace,      "{") 
             , (Var,        "c")
             , (Asm,        ":=")
             , (Bracket,    "(")
             , (Nmbr,       "2.5")
             , (Op,         "+")
             , (Nmbr,       "8")
             , (Bracket,    ")")
             , (Brace,      "}")       
             , (Var,        "b")
             , (Asm,        ":=")
             , (Bracket,    "(")
             , (Nmbr,       "2.5")
             , (Op,         "+")
             , (Nmbr,       "8")
             , (Bracket,    ")")
             , (Brace,      "}")
             ]

toTokenList :: [(Alphabet, [Char])] -> [Token]
toTokenList tl = zipWith (toTokenList') tl [0..]

toTokenList' :: (Alphabet, [Char]) -> Int -> Token
toTokenList' (a, chrs) n = (a, chrs, n) 

tokenToString :: [Token] -> String
tokenToString [(_,s,_)]     = s
tokenToString ((_,s,_):tks) = s ++ tokenToString tks

-- Parse this tokenlist with a call to the function parse, with
--      - grammar: the name of the grammar above
--      - Expr: the start-nonterminal of the grammar above
--      - tokenList0: the tokenlist above
parseTree0 = parse grammar Expr tokenList0

-- prpr: for pretty-printing the parsetree, including error messages
testTxt    = prpr parseTree0

-- showTree + toRoseTree: for graphical representation in browser
testGr     = showTree $ toRoseTree parseTree0

testShow2  = showTree $ astToRose $ ptreeToAst $ parse grammar Program $ resWord $ toTokenList tokenList2


