module LLang where

import AST (AST (..), Operator (..), Subst)
import Combinators (Parser (..), Result (..), elem', fail',
                    satisfy, success, symbol, symbols)
import Expr (Associativity (..), OpType (..), parseNum,
             toOperator, parseOp, evalExpr, uberExpr)
import Data.Char (isDigit, isLetter, isSpace)
import Control.Applicative
import Control.Monad (guard)
import Data.Maybe
import qualified Data.Map as Map

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

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

someSpaces = some $ satisfy isSpace
manySpaces = many $ satisfy isSpace

parseVar' = do
  someSpaces
  var <- parseVar
  return var

parseExpr'' = do
  manySpaces
  symbol '('
  manySpaces
  expr <- parseExpr'
  manySpaces
  symbol ')'
  return expr

parseIf :: Parser String String LAst
parseIf = do
  symbols "if"
  expr <- parseExpr''
  ins  <- parseSeq
  ins' <- parseSeq
  return $ If expr ins ins'

parseWhile :: Parser String String LAst
parseWhile = do
  symbols "while"
  expr <- parseExpr''
  ins  <- parseSeq
  return $ While expr ins

parseAssign :: Parser String String LAst
parseAssign = do
  symbols "bind"
  var  <- parseVar'
  expr <- parseExpr''
  return $ Assign var expr

parseRead :: Parser String String LAst
parseRead = do
  symbols "read"
  var <- parseVar'
  return $ Read var

parseWrite :: Parser String String LAst
parseWrite = do
  symbols "write"
  expr <- parseExpr''
  return $ Write expr

parseSeq :: Parser String String LAst
parseSeq = do
  manySpaces
  symbols "./"
  manySpaces
  ins <- many (parseIns <* manySpaces <* symbol ';' <* manySpaces)
  symbols "\\."
  return $ Seq ins

parseIns :: Parser String String LAst
parseIns = parseIf <|> parseWhile <|> parseAssign <|> parseRead <|> parseWrite <|> parseSeq

exprVars :: Expr -> [Var]
exprVars (Num _)              = []
exprVars (Ident x)            = [x]
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

parseL :: Parser String String LAst
parseL = Parser $ \str ->
  let
    short = runParser parseSeq str
  in
    case short of
      this@(Success _ ast) ->
        case goodVars ast of
          True -> this
          _    -> Failure "Unassigned variable"
      _        -> short

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval (If cond ins ins') conf@(Conf subst in' out') = do
  res <- evalExpr subst cond
  case res of
    0 -> eval ins' conf
    _ -> eval ins  conf
eval loop@(While cond body) conf@(Conf subst in' out') = do
  res <- evalExpr subst cond
  case res of
    0 -> return conf
    _ -> do
      res' <- eval body conf
      eval loop res'
eval (Assign var expr) (Conf subst in' out') = do
  res <- evalExpr subst expr
  return $ Conf (Map.insert var res subst) in' out'
eval (Read var) (Conf subst in' out') =
  case in' of
    (x:xs) -> return $ Conf (Map.insert var x subst) xs out'
    _      -> Nothing
eval (Write expr) (Conf subst in' out') = do
  res <- evalExpr subst expr
  return $ Conf subst in' (res:out')
eval (Seq instructions) conf =
  case instructions of
    (x:xs) -> do
      res <- eval x conf
      eval (Seq xs) res
    _      -> Just conf
