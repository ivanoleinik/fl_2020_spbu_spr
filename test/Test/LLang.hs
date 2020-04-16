module Test.LLang where

import           AST
import           Combinators      (Parser (..), Result (..), runParser, toStream)
import qualified Data.Map         as Map
import           Debug.Trace      (trace)
import           LLang
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)

-- f x y = read z ; return (x + z * y)
-- g x = if (x) then return x else return x*13
-- {read x; read y; write (f x y); write (g x)}"

prog =
  Program
    [ Function "f" ["x", "y"] (Seq [Read "z", Return (BinOp Plus (Ident "x") (Ident "y"))])
    , Function "g" ["x"] (If (Ident "x") (Return (Ident "x")) (Return (BinOp Mult (Ident "x") (Num 13))))
    ]
    (
      Seq
        [ Read "x"
        , Read "y"
        , Write (FunctionCall "f" [Ident "x", Ident "y"])
        , Write (FunctionCall "g" [Ident "x"])
        ]
    )

isFailure (Failure _) = True
isFailure  _          = False

unit_parseVar :: Assertion
unit_parseVar = do
  runParser parseVar "_data" @?= Success "" "_data"
  runParser parseVar "foo\'\'" @?= Success "" "foo\'\'"
  runParser parseVar "Zipp077" @?= Success "" "Zipp077"
  runParser parseVar "a_1_b_2 c_3" @?= Success " c_3" "a_1_b_2"
  assertBool "" $ isFailure (runParser parseVar "1c")
  assertBool "" $ isFailure (runParser parseVar "\'_\'")
  assertBool "" $ isFailure (runParser parseVar "abcdefghijk")
  assertBool "" $ isFailure (runParser parseVar "")
  assertBool "" $ isFailure (runParser parseVar "while")

unit_parseExpr' :: Assertion
unit_parseExpr' = do
  runParser parseExpr' "1*2*3"   @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
  runParser parseExpr' "123"     @?= Success "" (Num 123)
  runParser parseExpr' "abc"     @?= Success "" (Ident "abc")
  runParser parseExpr' "1*2+3*4" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (BinOp Mult (Num 3) (Num 4)))
  runParser parseExpr' "1+2*3+4" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Num 2) (Num 3))) (Num 4))
  runParser parseExpr' "1*x*3"   @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Ident "x")) (Num 3))
  runParser parseExpr' "xyz"     @?= Success "" (Ident "xyz")
  runParser parseExpr' "1*x+z*4" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Ident "x")) (BinOp Mult (Ident "z") (Num 4)))
  runParser parseExpr' "1+y*3+z" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Ident "y") (Num 3))) (Ident "z"))
  runParser parseExpr' "1+x" @?= Success "" (BinOp Plus (Num 1) (Ident "x"))
  runParser parseExpr' "1-x" @?= Success "" (BinOp Minus (Num 1) (Ident "x"))
  runParser parseExpr' "1*x" @?= Success "" (BinOp Mult (Num 1) (Ident "x"))
  runParser parseExpr' "1/x" @?= Success "" (BinOp Div (Num 1) (Ident "x"))
  runParser parseExpr' "1^x" @?= Success "" (BinOp Pow (Num 1) (Ident "x"))
  runParser parseExpr' "1==x" @?= Success "" (BinOp Equal (Num 1) (Ident "x"))
  runParser parseExpr' "1/=x" @?= Success "" (BinOp Nequal (Num 1) (Ident "x"))
  runParser parseExpr' "1>x" @?= Success "" (BinOp Gt (Num 1) (Ident "x"))
  runParser parseExpr' "1>=x" @?= Success "" (BinOp Ge (Num 1) (Ident "x"))
  runParser parseExpr' "1<x" @?= Success "" (BinOp Lt (Num 1) (Ident "x"))
  runParser parseExpr' "1<=x" @?= Success "" (BinOp Le (Num 1) (Ident "x"))
  runParser parseExpr' "1&&x" @?= Success "" (BinOp And (Num 1) (Ident "x"))
  runParser parseExpr' "1||x" @?= Success "" (BinOp Or (Num 1) (Ident "x"))
  runParser parseExpr' "1||x\'" @?= Success "" (BinOp Or (Num 1) (Ident "x\'"))
  runParser parseExpr' "(1==x+2)||3*4<y-5/6&&(7/=z^8)||(id>12)&&abc<=13||xyz>=42" @?=
      runParser parseExpr' "(1==(x+2))||(((3*4)<(y-(5/6))&&(7/=(z^8)))||(((id>12)&&(abc<=13))||(xyz>=42)))"
  runParser parseExpr' "123" @?= Success "" (Num 123)
  runParser parseExpr' "abcd" @?= Success "" (Ident "abcd")
  runParser parseExpr' "(w^h^o')" @?=
    Success "" (BinOp Pow (Ident "w") (BinOp Pow (Ident "h") (Ident "o\'")))
  runParser parseExpr' "!(!x)" @?= Success "" (UnaryOp Not (UnaryOp Not (Ident "x")))
  assertBool "" $ isFailure (runParser parseExpr' "+2-a")
  assertBool "" $ isFailure (runParser parseExpr' "-----5")
  assertBool "" $ isFailure (runParser parseExpr' "(')_(')")
  assertBool "" $ isFailure (runParser parseExpr' "!!x")

unit_parseIf :: Assertion
unit_parseIf = do
  runParser parseIf "if(1)./\\../\\." @?=
    Success "" (If
                   { cond = (Num 1)
                   , thn  = Seq []
                   , els  = Seq []
                   }
               )
  runParser parseIf "if (x>=0) ./ write (1); \\. ./ write (-1); \\." @?=
    Success "" (If
                   { cond = BinOp Ge (Ident "x") (Num 0)
                   , thn  = Seq [Write (Num 1)]
                   , els  = Seq [Write (UnaryOp Minus (Num 1))]
                   }
               )
  runParser parseIf "if (x/=y) ./ read a; write(a); \\. ./ \\.kek" @?=
    Success "kek" (If
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
  runParser parseWhile "while(1)\n./\n\\." @?=
    Success "" (While
                   { cond = (Num 1)
                   , body = Seq []
                   }
               )
  runParser parseWhile "while (x<0) ./ write (1); \\." @?=
    Success "" (While
                   { cond = BinOp Lt (Ident "x") (Num 0)
                   , body = Seq [Write (Num 1)]
                   }
               )
  assertBool "" $ isFailure (runParser parseWhile "while a ./\\.")
  assertBool "" $ isFailure (runParser parseWhile "while (a) ./ write (a) \\.")
  assertBool "" $ isFailure (runParser parseWhile "while (!!a) ./\\.")
  assertBool "" $ isFailure (runParser parseWhile "while () ./\\.")

unit_parseAssign :: Assertion
unit_parseAssign = do
  runParser parseAssign "bind x (1+1)" @?=
    Success "" (Assign "x" (BinOp Plus (Num 1) (Num 1)))
  runParser parseAssign "bind x\' (x||!x)" @?=
    Success "" (Assign "x\'" (BinOp Or (Ident "x") (UnaryOp Not (Ident "x"))))
  runParser parseAssign "bind _lol_ (kek\'\')..." @?=
    Success "..." (Assign "_lol_" (Ident "kek\'\'"))
  assertBool "" $ isFailure (runParser parseAssign "bind (a) 1./\\.")
  assertBool "" $ isFailure (runParser parseAssign "bind a 1")
  assertBool "" $ isFailure (runParser parseAssign "bind (a) (1)")
  assertBool "" $ isFailure (runParser parseAssign "bind -a (1)")
  assertBool "" $ isFailure (runParser parseAssign "bind if (1)")

unit_parseRead :: Assertion
unit_parseRead = do
  runParser parseRead "read a" @?= Success "" (Read "a")
  runParser parseRead "read _m_e_m_" @?= Success "" (Read "_m_e_m_")
  runParser parseRead "read x12345" @?= Success "" (Read "x12345")
  assertBool "" $ isFailure (runParser parseRead "read (x)")
  assertBool "" $ isFailure (runParser parseRead "read write")
  assertBool "" $ isFailure (runParser parseRead "Read a")
  assertBool "" $ isFailure (runParser parseRead "read ((a))")
  assertBool "" $ isFailure (runParser parseRead "read \n\t\n")

unit_parseWrite :: Assertion
unit_parseWrite = do
  runParser parseWrite "write (a)" @?= Success "" (Write (Ident "a"))
  runParser parseWrite "write (2/34)" @?=
    Success "" (Write (BinOp Div (Num 2) (Num 34)))
  runParser parseWrite "write (x^y)-1" @?=
    Success "-1" (Write (BinOp Pow (Ident "x") (Ident "y")))
  assertBool "" $ isFailure (runParser parseWrite "write x")
  assertBool "" $ isFailure (runParser parseWrite "write (x-1-)")
  assertBool "" $ isFailure (runParser parseWrite "write 1")
  assertBool "" $ isFailure (runParser parseWrite "write (1a0)")
  assertBool "" $ isFailure (runParser parseWrite "write ()")

unit_parseSeq :: Assertion
unit_parseSeq = do
  runParser parseSeq "./\\." @?= Success "" (Seq [])
  runParser parseSeq "././\\.;././\\.;./\\.;\\.;\\.\n\n\n" @?=
    Success "" (Seq [Seq [], Seq[Seq[], Seq []]])
  runParser parseSeq "./ bind a (4*5); \\." @?=
    Success "" (Seq [ Assign "a" (BinOp Mult (Num 4) (Num 5))])
  assertBool "" $ isFailure (runParser parseSeq "./;\\.")
  assertBool "" $ isFailure (runParser parseSeq "./ bind (a) (4*5); \\.")
  assertBool "" $ isFailure (runParser parseSeq "././\\.")
  assertBool "" $ isFailure (runParser parseSeq "./")
  assertBool "" $ isFailure (runParser parseSeq "")

unit_parseL :: Assertion
unit_parseL = do
  runParser parseL "./ read a; bind b (1+2*3); bind result (0); while (a>b) ./ bind result (result+10); bind a (a-1); \\.; write (result); \\." @?=
    Success "" (Seq
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
  runParser parseL "./ bind abc123 (23); \\." @?=
    Success "" (Seq [Assign "abc123" (Num 23)])
  assertBool "" $ isFailure (runParser parseL "./ read a; write (b); \\.")
  assertBool "" $ isFailure (runParser parseL "./ bind a (b); \\.")
  assertBool "" $ isFailure (runParser parseL "./ bind a (a); \\.")
  assertBool "" $ isFailure (runParser parseL "./ write (a); \\.")
  assertBool "" $ isFailure (runParser parseL "./ if (a==a) ./\\. write (a); ./\\. \\.")
  assertBool "" $ isFailure (runParser parseL "./ while (a) ./ write (a); \\. \\.")
  assertBool "" $ isFailure (runParser parseL "./ bind abc 123 (23); \\.")
  assertBool "" $ isFailure (runParser parseL "./ b ind abc123 (23); \\.")
  assertBool "" $ isFailure (runParser parseL "./ bind abc123 (23); write (abc 123)\\.")

-- read x;
-- if (x > 13)
-- then { write x }
-- else {
--     while (x < 42) {
--       x := x * 7;
--       write (x);
--     }
-- }
stmt1 :: LAst
stmt1 =
  Seq
    [ Read "x"
    , If (BinOp Gt (Ident "x") (Num 13))
         (Seq [(Write (Ident "x"))])
         (Seq [(While (BinOp Lt (Ident "x") (Num 42))
                (Seq [ Assign "x"
                        (BinOp Mult (Ident "x") (Num 7))
                     , Write (Ident "x")
                     ]
                )
         )])
    ]

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7])
  eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70])
  eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42])


-- read x;
-- if (x)
-- then {
--   while (x) {
--     x := x - 2;
--     write (x);
--   }
-- else {}
stmt2 :: LAst
stmt2 =
  Seq
    [ Read "x"
    , If (Ident "x")
         (Seq [(While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )])
         (Seq [])
    ]

unit_stmt2 :: Assertion
unit_stmt2 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [])
  eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0])
  eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]))

-- read x;
-- read y;
-- write (x == y);
stmt3 :: LAst
stmt3 =
  Seq
    [ Read "x"
    , Read "y"
    , Write (BinOp Equal (Ident "x") ((Ident "y")))
    ]

unit_stmt3 :: Assertion
unit_stmt3 = do
  let subst x y = Map.fromList [("x", x), ("y", y) ]
  eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0])
  eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1])
  eval stmt3 (initialConf [42]) @?= Nothing

-- read n;
-- if (n == 1 || n == 2)
-- then {
--   write 1;
-- }
-- else {
--   i := 2;
--   cur := 1
--   prev := 1
--   while (i < n) {
--     temp := cur + prev;
--     prev := cur;
--     cur := temp;
--     i := i + 1;
--   }
--   write (cur);
-- }
stmt4 :: LAst
stmt4 =
  Seq
    [ Read "n"
    , If (BinOp Or (BinOp Equal (Ident "n") (Num 1)) (BinOp Equal (Ident "n") (Num 2)))
         (Seq [(Write (Num 1))])
         (Seq
            [ Assign "i" (Num 2)
            , Assign "cur" (Num 1)
            , Assign "prev" (Num 1)
            , While (BinOp Lt (Ident "i") (Ident "n"))
                     (Seq
                        [ Assign "temp" (BinOp Plus (Ident "cur") (Ident "prev"))
                        , Assign "prev" (Ident "cur")
                        , Assign "cur" (Ident "temp")
                        , Assign "i" (BinOp Plus (Ident "i") (Num 1))
                        ]
                     )
            , Write (Ident "cur")
            ]
         )
    ]

unit_stmt4 :: Assertion
unit_stmt4 = do
  let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
  let subst' n = Map.fromList [("n", n)]
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
  eval stmt4 (initialConf []) @?= Nothing
