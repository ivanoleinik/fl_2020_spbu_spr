module LLang where

import AST (AST (..), Operator (..))
import Combinators (Parser (..), Result (..), elem', fail',
                    satisfy, success, symbol, symbols)
import Expr (Associativity (..), OpType (..), parseNum,
             toOperator, parseOp, uberExpr)
import Data.Char (isDigit, isLetter)
import Control.Applicative
import Control.Monad (guard)
import Data.Maybe

type Expr = AST

type Var = String

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

plus  = symbols "+"  >>= toOperator
minus = symbols "-"  >>= toOperator
mult  = symbols "*"  >>= toOperator
div'  = symbols "/"  >>= toOperator
pow   = symbols "^"  >>= toOperator
eq    = symbols "==" >>= toOperator
neq   = symbols "/=" >>= toOperator
le    = symbols "<=" >>= toOperator
lt    = symbols "<"  >>= toOperator
ge    = symbols ">=" >>= toOperator
gt    = symbols ">"  >>= toOperator
not'  = symbols "!"  >>= toOperator
and'  = symbols "&&" >>= toOperator
or'   = symbols "||" >>= toOperator

parseIdent' :: Parser String String String
parseIdent' = do
  x <- some $ (satisfy isLetter) <|> (symbol '_')
  y <- many $ (satisfy isLetter) <|> (symbol '_') <|> (satisfy isDigit)
  z <- many $ symbol '\''
  return (x ++ y ++ z)

keywords :: [String]
keywords = ["if", "while", "bind", "read", "write"]

parseVar :: Parser String String String
parseVar = do
  var <- parseIdent'
  guard $ length var <= 10
  guard $ notElem var keywords
  return var

parseExpr' :: Parser String String AST
parseExpr' = uberExpr [ (or', Binary RightAssoc)
                      , (and', Binary RightAssoc)
                      , (not', Unary)
                      , (eq <|> neq <|> le <|> lt <|> ge <|> gt, Binary NoAssoc)
                      , (plus <|> minus, Binary LeftAssoc)
                      , (mult <|> div',  Binary LeftAssoc)
                      , (minus, Unary)
                      , (pow, Binary RightAssoc)
                      ]
                      (Num   <$> parseNum <|>
                       Ident <$> parseVar <|>
                       symbol '(' *> parseExpr' <* symbol ')'
                      )
                      BinOp
                      UnaryOp

parseIf :: Parser String String LAst
parseIf = do
  symbols "if("
  expr <- parseExpr'
  symbols ")"
  ins  <- parseSeq
  ins' <- parseSeq
  return $ If expr ins ins'

parseWhile :: Parser String String LAst
parseWhile = do
  symbols "while("
  expr <- parseExpr'
  symbols ")"
  ins  <- parseSeq
  return $ While expr ins

parseAssign :: Parser String String LAst
parseAssign = do
  symbols "bind("
  var  <- parseVar
  symbols ")("
  expr <- parseExpr'
  symbol ')'
  return $ Assign var expr

parseRead :: Parser String String LAst
parseRead = do
  symbols "read("
  var <- parseVar
  symbol ')'
  return $ Read var

parseWrite :: Parser String String LAst
parseWrite = do
  symbols "write("
  expr <- parseExpr'
  symbol ')'
  return $ Write expr

parseSeq :: Parser String String LAst
parseSeq = do
  symbols "./"
  ins <- many (parseIns <* symbol ';')
  symbols "\\."
  return $ Seq ins

parseIns :: Parser String String LAst
parseIns = parseIf <|> parseWhile <|> parseAssign <|> parseRead <|> parseWrite <|> parseSeq

exprVars :: Expr -> [Var]
exprVars (Num _)     = []
exprVars (Ident x) = [x]
exprVars (UnaryOp _ expr)     = exprVars expr
exprVars (BinOp _ expr expr') = exprVars expr ++ exprVars expr'

goodVars :: LAst -> Bool
goodVars ast =
  let
    in' vars vars' = all (\var -> elem var vars') vars
    dfs (If cond ins ins') vars =
      case in' (exprVars cond) vars &&
           isJust (dfs ins  vars)  &&
           isJust (dfs ins' vars) of
        True -> Just vars
        _    -> Nothing
    dfs (While cond body) vars =
      case in' (exprVars cond) vars &&
           isJust (dfs body vars) of
        True -> Just vars
        _    -> Nothing
    dfs (Assign var expr) vars =
      case in' (exprVars expr) vars of
        True -> Just $
          case elem var vars of
            True -> vars
            _    -> (var:vars)
        _    -> Nothing
    dfs (Read var) vars = Just $
      case elem var vars of
        True -> vars
        _    -> (var:vars)
    dfs (Write expr) vars =
      case in' (exprVars expr) vars of
        True -> Just vars
        _    -> Nothing
    dfs (Seq instructions) vars =
      let
        f (Just vars) ins = dfs ins vars
        f Nothing ins = Nothing
      in
        foldl f (Just vars) instructions
  in
    isJust (dfs ast [])

deleteSpaces :: (Parser String String LAst) -> (Parser String String LAst)
deleteSpaces p = Parser $ runParser p . deleteSpaces'
deleteSpaces' = concat . words

parseL :: Parser String String LAst
parseL = Parser $ \str ->
  let
    short = runParser parseSeq $ deleteSpaces' str
  in
    case short of
      this@(Success _ ast) ->
        case goodVars ast of
          True -> this
          _    -> Failure "Unassigned variable"
      _        -> short