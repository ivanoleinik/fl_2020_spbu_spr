module Test.Combinators where

import           Combinators         (Parser, Result (..), Position (..),
                                      runParser, satisfy, symbol,
                                      toStream,)
import           Control.Applicative
import           Test.Tasty.HUnit    (Assertion, assertBool, (@?=))

testFailure = assertBool "" . isFailure

isFailure (Failure _) = True
isFailure _           = False

digit :: Parser String String Char
digit = satisfy (`elem` "0123456789")

unit_satisfy :: Assertion
unit_satisfy = do
    testFailure $ runParser (satisfy (/= '1')) "1234"
    runParser (satisfy (== '1')) "1234" @?= Success (toStream "234" (Position 0 1)) '1'
    runParser digit "1234" @?= Success (toStream "234" (Position 0 1)) '1'
    testFailure $ runParser digit "blah"

unit_many :: Assertion
unit_many = do
    runParser (many $ symbol '1') "234" @?= Success (toStream "234" (Position 0 0)) ""
    runParser (many $ symbol '1') "134" @?= Success (toStream "34" (Position 0 1)) "1"
    runParser (many $ symbol '1') "114" @?= Success (toStream "4" (Position 0 2)) "11"
    runParser (many $ symbol '1') "111" @?= Success (toStream "" (Position 0 3))"111"

unit_some :: Assertion
unit_some = do
    testFailure $ runParser (some $ symbol '1') "234"
    runParser (some $ symbol '1') "134" @?= Success (toStream "34" (Position 0 1)) "1"
    runParser (some $ symbol '1') "114" @?= Success (toStream "4" (Position 0 2)) "11"
    runParser (some $ symbol '1') "111" @?= Success (toStream "" (Position 0 3))"111"
