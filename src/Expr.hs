module Expr where

import           AST                 (AST (..), Operator (..), Subst (..))
import           Combinators         (Parser (..), Result (..), elem', fail',
                                      runParser, satisfy, stream, success, symbol, symbols)
import           Control.Applicative
import           Data.Char           (digitToInt, isDigit, isLetter)
import qualified Data.Map            as Map

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary

evalExpr :: Subst -> AST -> Maybe Int
evalExpr _     (Num x)        = Just x
evalExpr subst (Ident x)      = Map.lookup x subst
evalExpr subst (UnaryOp op x) = do
                                z <- evalExpr subst x
                                return $ compute $ UnaryOp op (Num z)
evalExpr subst (BinOp op x y) = do
                                z <- evalExpr subst x
                                t <- evalExpr subst y
                                return $ compute $ BinOp op (Num z) (Num t)

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr [] elp _ _ = elp
uberExpr ((opp, assoc):pars) elp bin un =
  let rest = uberExpr pars elp bin un
  in case assoc of
    Unary -> un <$> opp <*> rest <|> rest
    Binary NoAssoc    -> do
        l <- rest
        op <- opp
        r <- rest
        return $ bin op l r
      <|> rest
    Binary LeftAssoc  -> do
        (f, s) <- (,) <$> rest <*> (many ((,) <$> opp <*> rest))
        return $ foldl (\l (op, r) -> bin op l r) f s
      <|> rest
    Binary RightAssoc -> do
        (f, s) <- (,) <$> (many ((,) <$> rest <*> opp)) <*> rest
        return $ foldr (\(r, op) l -> bin op r l) s f
      <|> rest

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

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [ (or', Binary RightAssoc)
                     , (and', Binary RightAssoc)
                     , (not', Unary)
                     , (eq <|> neq <|> le <|> lt <|> ge <|> gt, Binary NoAssoc)
                     , (plus <|> minus, Binary LeftAssoc)
                     , (mult <|> div',  Binary LeftAssoc)
                     , (minus, Unary)
                     , (pow, Binary RightAssoc)
                     ]
                     (Num   <$> parseNum   <|>
                      Ident <$> parseIdent <|>
                      symbol '(' *> parseExpr <* symbol ')'
                     )
                     BinOp
                     UnaryOp

-- Парсер для целых чисел
parseNegNum :: Parser String String Int
parseNegNum = foldl ev 0 <$> go
  where
    ev acc '-' = -acc
    ev acc x   = 10 * acc + digitToInt x
    go :: Parser String String String
    go = some (satisfy isDigit) <|>
         flip (++) <$> many (symbol '-') <*> some (satisfy isDigit)

-- Парсер для неотрицательных целых чисел
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go
  where
    go :: Parser String String String
    go = some (satisfy isDigit)

parseIdent :: Parser String String String
parseIdent = do
  x <- some $ (satisfy isLetter) <|> (symbol '_')
  y <- many $ (satisfy isLetter) <|> (symbol '_') <|> (satisfy isDigit)
  return (x ++ y)

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = (parse operators) >>= toOperator
  where
    parse [s]    =  symbols s
    parse (s:xs) = (symbols s) <|> (parse xs)
    operators = ["+", "-", "*", "/", "^",
                 "==", "/=", "<=", "<", ">=", ">",
                 "!", "&&", "||"]

-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+"  = success Plus
toOperator "*"  = success Mult
toOperator "-"  = success Minus
toOperator "/"  = success Div
toOperator "^"  = success Pow
toOperator "==" = success Equal
toOperator "/=" = success Nequal
toOperator "<=" = success Le
toOperator "<"  = success Lt
toOperator ">=" = success Ge
toOperator ">"  = success Gt
toOperator "!"  = success Not
toOperator "&&" = success And
toOperator "||" = success Or
toOperator _    = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null (stream rest) -> return $ compute ast
    _                                     -> Nothing

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt _    = 0

compute :: AST -> Int
compute (Num x)           = x
compute (UnaryOp Minus x) = - compute x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y
compute (BinOp Pow x y)   = compute x ^ compute y
compute (BinOp Equal x y)  = boolToInt $ compute x == compute y
compute (BinOp Nequal x y) = boolToInt $ compute x /= compute y
compute (BinOp Gt x y)     = boolToInt $ compute x > compute y
compute (BinOp Le x y)     = boolToInt $ compute x <= compute y
compute (BinOp Ge x y)     = boolToInt $ compute x >= compute y
compute (BinOp Lt x y)     = boolToInt $ compute x < compute y
compute (UnaryOp Not x)    = case compute x of
                                0 -> 1
                                _ -> 0
compute (BinOp And x y)    = case compute x of
                                0 -> 0
                                _ -> compute y
compute (BinOp Or x y)     = case compute x of
                                0 -> compute y
                                x -> x
