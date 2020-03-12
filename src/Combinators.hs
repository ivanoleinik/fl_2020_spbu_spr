module Combinators where

import Control.Applicative

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result }

instance Functor (Parser error input) where
  fmap f (Parser rp) = Parser $ \input ->
    case rp input of
      Success x y -> Success x (f y)
      Failure e   -> Failure e

instance Applicative (Parser error input) where
  pure x = Parser $ \input -> Success input x
  (Parser rp1) <*> (Parser rp2) = Parser $ \input ->
    case rp1 input of
      Success input' f ->
        case rp2 input' of
          Success input'' x -> Success input'' (f x)
          Failure e         -> Failure e
      Failure e             -> Failure e

instance Monad (Parser error input) where
  return = pure
  (Parser rp) >>= f = Parser $ \input ->
    case rp input of
      Success input' x -> runParser (f x) input'
      Failure e        -> Failure e

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \input -> Failure mempty
  (Parser rp1) <|> (Parser rp2) = Parser $ \input ->
    case rp1 input of
      Failure _  -> rp2 input
      x          -> x

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem <*> (many (sep *> elem))

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Char -> Parser String String Char
symbol c = satisfy (== c)

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: (Show a) => Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: Show a => (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \input ->
  case input of
    (x:xs) | p x -> Success xs x
    []           -> Failure $ "Empty string"
    (x:xs)       -> Failure $ "Predicate failed"

-- Успешно парсит пустую строку
epsilon :: Parser e i ()
epsilon = success ()

-- Всегда завершается успехом, вход не читает, возвращает данное значение
success :: a -> Parser e i a
success = pure

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' = Parser . const . Failure
