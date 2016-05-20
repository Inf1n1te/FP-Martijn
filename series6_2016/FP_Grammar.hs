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
                   ,[ rep, Expr, lBrace, (+:) [Stat], rBrace]
		           ,[ ifstr, lBrace, Expr, rBrace, thenstr, lBrace, (+:) [Stat], rBrace, elsestr, lBrace, (*:) [Stat], rBrace ]
                   ,[ Expr                                  ]]
        Program -> [[ lBrace, (+:) [Stat], rBrace           ]]


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

tokenList2 :: [Token]             
tokenList2 = [ (Nmbr, "4",0) ]

-- Parse this tokenlist with a call to the function parse, with
--      - grammar: the name of the grammar above
--      - Expr: the start-nonterminal of the grammar above
--      - tokenList0: the tokenlist above
parseTree0 = parse grammar Expr tokenList0

-- prpr: for pretty-printing the parsetree, including error messages
testTxt    = prpr parseTree0

-- showTree + toRoseTree: for graphical representation in browser
testGr     = showTree $ toRoseTree parseTree0


