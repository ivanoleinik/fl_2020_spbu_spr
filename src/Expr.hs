module Expr where

import AST (AST (..), Operator (..))
import Combinators (Parser (..), Result (..), elem', fail',
                    satisfy, success, satisfy, symbol, symbols)
import Data.Char (digitToInt, isDigit, isLetter)
import Control.Applicative

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

-- Универсальный парсер выражений
uberExpr :: Monoid e
         => [(Parser e i op, Associativity)] -- список парсеров бинарных операторов с ассоциативностями в порядке повышения приоритета
         -> Parser e i ast -- парсер для элементарного выражения
         -> (op -> ast -> ast -> ast) -- функция для создания абстрактного синтаксического дерева для бинарного оператора
         -> Parser e i ast
uberExpr [] elp _ = elp
uberExpr ((opp, assoc):pars) elp build =
  let rest = uberExpr pars elp build
  in case assoc of
    NoAssoc   -> do
        l <- rest
        op <- opp
        r <- rest
        return $ build op l r
      <|> rest
    LeftAssoc -> do
      (f, s) <- (,) <$> rest <*> (many ((,) <$> opp <*> rest))
      return $ foldl (\l (op, r) -> build op l r) f s
    RightAssoc  -> do
      (f, s) <- (,) <$> (many ((,) <$> rest <*> opp)) <*> rest
      return $ foldr (\(r, op) l -> build op r l) s f

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
and'  = symbols "&&" >>= toOperator
or'   = symbols "||" >>= toOperator

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [ (or', RightAssoc)
                     , (and', RightAssoc)
                     , (eq <|> neq <|> le <|> lt <|> ge <|> gt, NoAssoc)
                     , (plus <|> minus, LeftAssoc)
                     , (mult <|> div',  LeftAssoc)
                     , (pow, RightAssoc)
                     ]
                     (Num   <$> parseNum   <|>
                      Ident <$> parseIdent <|>
                      symbol '(' *> parseExpr <* symbol ')'
                     )
                     BinOp

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = foldl ev 0 <$> go
  where
    ev acc '-' = -acc
    ev acc x   = 10 * acc + digitToInt x
    go :: Parser String String String
    go = some (satisfy isDigit) <|> ((flip (++)) <$> many (symbol '-') <*> some (satisfy isDigit))

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
                 "&&", "||"]

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
toOperator "&&" = success And
toOperator "||" = success Or
toOperator _    = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y
