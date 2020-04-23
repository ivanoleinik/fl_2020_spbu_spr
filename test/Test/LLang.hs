module Test.LLang where

import           AST
import           Combinators      (Parser (..), Result (..), runParser, toStream)
import qualified Data.Map         as Map
import           Debug.Trace      (trace)
import           LLang
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)

isFailure (Failure _) = True
isFailure  _          = False

shrek :: (Eq a, Show a) => Parser String String a -> String -> String -> a -> Assertion
shrek p input r a = do
    runParser p input @?= Success (toStream r (length input - length r)) a

unit_parseVar :: Assertion
unit_parseVar = do
  shrek parseVar "_data" "" "_data"
  shrek parseVar "foo\'\'" "" "foo\'\'"
  shrek parseVar "Zipp077" "" "Zipp077"
  shrek parseVar "a_1_b_2 c_3" " c_3" "a_1_b_2"
  assertBool "" $ isFailure (runParser parseVar "1c")
  assertBool "" $ isFailure (runParser parseVar "\'_\'")
  assertBool "" $ isFailure (runParser parseVar "")
  assertBool "" $ isFailure (runParser parseVar "while")

unit_parseFunctionCall :: Assertion
unit_parseFunctionCall = do
  shrek parseFunctionCall "foo(x)" "" (FunctionCall "foo" [Ident "x"])
  shrek parseFunctionCall "kek(300, 1)" "" (FunctionCall "kek" [Num 300, Num 1])
  shrek parseFunctionCall "lol(2)" "" (FunctionCall "lol" [Num 2])
  shrek parseFunctionCall "mem()" "" (FunctionCall "mem" [])
  assertBool "" $ isFailure (runParser parseFunctionCall "fun()")
  assertBool "" $ isFailure (runParser parseFunctionCall "foo")
  assertBool "" $ isFailure (runParser parseFunctionCall "bar(a b)")

unit_parseExpr' :: Assertion
unit_parseExpr' = do
  shrek parseExpr' "1 *2*3"   "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
  shrek parseExpr' "123"     "" (Num 123)
  shrek parseExpr' "abc "     "" (Ident "abc")
  shrek parseExpr' "1*2 +3* 4" "" (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (BinOp Mult (Num 3) (Num 4)))
  shrek parseExpr' "1 + 2 * 3 + 4" "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Num 2) (Num 3))) (Num 4))
  shrek parseExpr' "1* x* 3"   "" (BinOp Mult (BinOp Mult (Num 1) (Ident "x")) (Num 3))
  shrek parseExpr' "xyz"     "" (Ident "xyz")
  shrek parseExpr' "1* x+z *4" "" (BinOp Plus (BinOp Mult (Num 1) (Ident "x")) (BinOp Mult (Ident "z") (Num 4)))
  shrek parseExpr' " 1 +y*3+z " "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Ident "y") (Num 3))) (Ident "z"))
  shrek parseExpr' "1+ x"     "" (BinOp Plus (Num 1) (Ident "x"))
  shrek parseExpr' "1 -x"     "" (BinOp Minus (Num 1) (Ident "x"))
  shrek parseExpr' "1*  x"     "" (BinOp Mult (Num 1) (Ident "x"))
  shrek parseExpr' "1/x"     "" (BinOp Div (Num 1) (Ident "x"))
  shrek parseExpr' "1^x"     "" (BinOp Pow (Num 1) (Ident "x"))
  shrek parseExpr' "1==x"    "" (BinOp Equal (Num 1) (Ident "x"))
  shrek parseExpr' "1/=x"    "" (BinOp Nequal (Num 1) (Ident "x"))
  shrek parseExpr' "1>x"     "" (BinOp Gt (Num 1) (Ident "x"))
  shrek parseExpr' "1>=x"    "" (BinOp Ge (Num 1) (Ident "x"))
  shrek parseExpr' "1<x"     "" (BinOp Lt (Num 1) (Ident "x"))
  shrek parseExpr' "1<=x"    "" (BinOp Le (Num 1) (Ident "x"))
  shrek parseExpr' "1&&x"    "" (BinOp And (Num 1) (Ident "x"))
  shrek parseExpr' "1||x"    "" (BinOp Or (Num 1) (Ident "x"))
  shrek parseExpr' "1||x\'"  "" (BinOp Or (Num 1) (Ident "x\'"))
  shrek parseExpr' "123"     "" (Num 123)
  shrek parseExpr' "abcd"    "" (Ident "abcd")
  shrek parseExpr' "(w^h^o')""" (BinOp Pow (Ident "w") (BinOp Pow (Ident "h") (Ident "o\'")))
  shrek parseExpr' "!(!x)"   "" (UnaryOp Not (UnaryOp Not (Ident "x")))
  shrek parseExpr' "b(1, 3) * c(2)" "" (BinOp Mult (FunctionCall "b" [Num 1, Num 3]) (FunctionCall "c" [Num 2]))
  shrek parseExpr' "a ( a ) ^b ( b )" "" (BinOp Pow (FunctionCall "a" [Ident "a"]) (FunctionCall "b" [Ident "b"]))
  assertBool "" $ isFailure (runParser parseExpr' "+2-a")
  assertBool "" $ isFailure (runParser parseExpr' "-----5")
  assertBool "" $ isFailure (runParser parseExpr' "(')_(')")
  assertBool "" $ isFailure (runParser parseExpr' "!!x")
  assertBool "" $ isFailure (runParser parseExpr' " ")
  assertBool "" $ isFailure (runParser parseExpr' "")

unit_parseIf :: Assertion
unit_parseIf = do
  shrek parseIf "if(1)./\\../\\." ""
               (If
                   { cond = (Num 1)
                   , thn  = Seq []
                   , els  = Seq []
                   }
               )
  shrek parseIf "if (x>=0) ./ write (1); \\. ./ write (-1); \\." ""
               (If
                   { cond = BinOp Ge (Ident "x") (Num 0)
                   , thn  = Seq [Write (Num 1)]
                   , els  = Seq [Write (UnaryOp Minus (Num 1))]
                   }
               )
  shrek parseIf "if (x/=y) ./ read a; write(a); \\. ./ \\.kek" "kek"
                  (If
                      { cond = BinOp Nequal (Ident "x") (Ident "y")
                      , thn  = Seq [Read "a", Write (Ident "a")]
                      , els  = Seq []
                      }
                  )
  assertBool "" $ isFailure (runParser parseIf "if()./\\../\\.")
  assertBool "" $ isFailure (runParser parseIf "if(1)./ read (a); \\. ./\\.")
  assertBool "" $ isFailure (runParser parseIf "if(1)./ read a \\. ./\\.")
  assertBool "" $ isFailure (runParser parseIf "if(1)./ read a; \\. ./\\")

unit_parseWhile :: Assertion
unit_parseWhile = do
  shrek parseWhile "while(1)\n./\n\\." ""
               (While
                   { cond = (Num 1)
                   , body = Seq []
                   }
               )
  shrek parseWhile "while (x < 0) ./ write (1); \\." ""
               (While
                   { cond = BinOp Lt (Ident "x") (Num 0)
                   , body = Seq [Write (Num 1)]
                   }
               )
  shrek parseWhile "while (a) ./\\." "" (While { cond = Ident "a", body = Seq []})
  assertBool "" $ isFailure (runParser parseWhile "while a ./ write (a); \\.")
  assertBool "" $ isFailure (runParser parseWhile "while (!!a) ./\\.")
  assertBool "" $ isFailure (runParser parseWhile "while () ./\\.")

unit_parseAssign :: Assertion
unit_parseAssign = do
  shrek parseAssign "bind x (1+1)" "" (Assign "x" (BinOp Plus (Num 1) (Num 1)))
  shrek parseAssign "bind x\' (x||!x)" "" (Assign "x\'" (BinOp Or (Ident "x") (UnaryOp Not (Ident "x"))))
  shrek parseAssign "bind _lol_ (kek\'\')..." "..." (Assign "_lol_" (Ident "kek\'\'"))
  shrek parseAssign "bind a (1)" "" (Assign "a" (Num 1))
  assertBool "" $ isFailure (runParser parseAssign "bind a 1./\\.")
  assertBool "" $ isFailure (runParser parseAssign "bind (a) (1)")
  assertBool "" $ isFailure (runParser parseAssign "bind -a (1)")
  assertBool "" $ isFailure (runParser parseAssign "bind if (1)")

unit_parseRead :: Assertion
unit_parseRead = do
  shrek parseRead "read a" "" (Read "a")
  shrek parseRead "read _m_e_m_" "" (Read "_m_e_m_")
  shrek parseRead "read x12345" "" (Read "x12345")
  assertBool "" $ isFailure (runParser parseRead "read (x)")
  assertBool "" $ isFailure (runParser parseRead "read write")
  assertBool "" $ isFailure (runParser parseRead "Read a")
  assertBool "" $ isFailure (runParser parseRead "read ((a))")
  assertBool "" $ isFailure (runParser parseRead "read \n\t\n")

unit_parseWrite :: Assertion
unit_parseWrite = do
  shrek parseWrite "write (a)" "" (Write (Ident "a"))
  shrek parseWrite "write (2/34)" "" (Write (BinOp Div (Num 2) (Num 34)))
  shrek parseWrite "write (x^y)-1" "-1" (Write (BinOp Pow (Ident "x") (Ident "y")))
  assertBool "" $ isFailure (runParser parseWrite "write x")
  assertBool "" $ isFailure (runParser parseWrite "write 1")
  assertBool "" $ isFailure (runParser parseWrite "write (x-1-)")
  assertBool "" $ isFailure (runParser parseWrite "write (1a0)")
  assertBool "" $ isFailure (runParser parseWrite "write ()")

unit_parseSeq :: Assertion
unit_parseSeq = do
  shrek parseSeq "./\\." "" (Seq [])
  shrek parseSeq "././\\.;././\\.;./\\.;\\.;\\.\n\n\n" "" (Seq [Seq [], Seq[Seq[], Seq []]])
  shrek parseSeq "./ bind a (4*5); \\." "" (Seq [ Assign "a" (BinOp Mult (Num 4) (Num 5))])
  assertBool "" $ isFailure (runParser parseSeq "./;\\.")
  assertBool "" $ isFailure (runParser parseSeq "./ bind (a) (4*5); \\.")
  assertBool "" $ isFailure (runParser parseSeq "././\\.")
  assertBool "" $ isFailure (runParser parseSeq "./")
  assertBool "" $ isFailure (runParser parseSeq "")

unit_parseReturn :: Assertion
unit_parseReturn = do
  shrek parseReturn "up (1+2)" "" (Return (BinOp Plus (Num 1) (Num 2)))
  shrek parseReturn "up (a)" "" (Return (Ident "a"))
  shrek parseReturn "up (foo(a, 5))" "" (Return (FunctionCall "foo" [Ident "a", Num 5]))
  assertBool "" $ isFailure (runParser parseReturn "up (up)")
  assertBool "" $ isFailure (runParser parseReturn "up 1")
  assertBool "" $ isFailure (runParser parseReturn "up a + 2")

unit_parseDef :: Assertion
unit_parseDef = do
  shrek parseDef "fun id: n ./ up (id(4)); \\." ""
    (Function "id" ["n"]
      (Seq [ Return (FunctionCall "id" [Num 4])
           , Return (Num 0)
           ]
      )
    )
  shrek parseDef "fun foo: a b ./ up (foo(a) + foo(b)); \\." ""
    (Function "foo" ["a", "b"]
      (Seq [ Return (BinOp Plus (FunctionCall "foo" [Ident "a"])
                                (FunctionCall "foo" [Ident "b"])
                    )
           , Return (Num 0)
           ]
      )
    )
  shrek parseDef "fun id: ./\\." "" (Function "id" [] (Seq [ Return (Num 0) ]))
  assertBool "" $ isFailure (runParser parseDef "fun f x ./ up (x); \\.")
  assertBool "" $ isFailure (runParser parseDef "fun f: x, y ./ up (x + y); \\.")
  assertBool "" $ isFailure (runParser parseDef "fun f: x ./ up (x) \\.")
  assertBool "" $ isFailure (runParser parseDef "fun f: x ./ up x; \\.")

unit_parseProg :: Assertion
unit_parseProg = do
  shrek parseProg "fun r: x ./ read x; \\. fun w: x ./ write(x); \\. ./ read x; write(r(x) || w(x)); \\." ""
    (Program [ Function "r" ["x"] (Seq [ Read "x", Return (Num 0)])
             , Function "w" ["x"] (Seq [ Write (Ident "x"), Return (Num 0)])
             ]
             (Seq [ Read "x"
                  , Write (BinOp Or (FunctionCall "r" [Ident "x"]) (FunctionCall "w" [Ident "x"]))
                  ]
             )
    )
  shrek parseProg "fun add: x y ./ up (x + y); \\. ./ write (add(2, 3)); \\." ""
    (Program [ Function "add" ["x", "y"] (Seq [ Return (BinOp Plus (Ident "x") (Ident "y"))
                                              , Return (Num 0)
                                              ]
                                         )
             ]
             (Seq [ Write (FunctionCall "add" [Num 2, Num 3])])
    )
  shrek parseProg "./ write(11); \\." ""
    (Program [] (Seq [ Write (Num 11)]))
  shrek parseProg "fun bar: a b c ./\\. ./\\." ""
    (Program [Function "bar" ["a", "b", "c"] (Seq [ Return (Num 0) ])] (Seq []))
  assertBool "" $ isFailure (runParser parseProg "fun kek: ./ write(x); \\. ./\\.")
  assertBool "" $ isFailure (runParser parseProg "fun if: x ./\\. ./\\.")
  assertBool "" $ isFailure (runParser parseProg "fun foo: a b ./ read a; write(a); \\. ./ write(b); \\.")
  assertBool "" $ isFailure (runParser parseProg "fun : x ./ up (x); \\. ./ return (0); \\.")

unit_parseL :: Assertion
unit_parseL = do
  shrek parseL "./ read a; bind b (1+2*3); bind result (0); while (a>b) ./ bind result (result+10); bind a (a-1); \\.; write (result); \\." ""
               (Seq
                   [ Read "a"
                   , Assign "b" (BinOp Plus (Num 1) (BinOp Mult (Num 2) (Num 3)))
                   , Assign "result" (Num 0)
                   , While
                       { cond = BinOp Gt (Ident "a") (Ident "b")
                       , body = Seq
                           [ Assign "result" (BinOp Plus (Ident "result") (Num 10))
                           , Assign "a" (BinOp Minus (Ident "a") (Num 1))
                           ]
                       }
                   , Write (Ident "result")
                   ]
               )
  shrek parseL "./ bind abc123 (23); \\." "" (Seq [Assign "abc123" (Num 23)])
  assertBool "" $ isFailure (runParser parseL "./ read a; write (b); \\.")
  assertBool "" $ isFailure (runParser parseL "./ bind a (b); \\.")
  assertBool "" $ isFailure (runParser parseL "./ bind a (a); \\.")
  assertBool "" $ isFailure (runParser parseL "./ write (a); \\.")
  assertBool "" $ isFailure (runParser parseL "./ if (a==a) ./\\. write (a); ./\\. \\.")
  assertBool "" $ isFailure (runParser parseL "./ while (a) ./ write (a); \\. \\.")
  assertBool "" $ isFailure (runParser parseL "./ bind abc 123 (23); \\.")
  assertBool "" $ isFailure (runParser parseL "./ b ind abc123 (23); \\.")
  assertBool "" $ isFailure (runParser parseL "./ bind abc123 (23); write (abc 123)\\.")
