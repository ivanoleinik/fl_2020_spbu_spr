module LEval where

import Combinators (Result (..), InputStream (..), runParser)
import LLang (Program (..), Configuration (..), Function (..), eval, parseProg)
import qualified Data.Map as Map

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg (Program functions main) inp =
  eval main               $
    Conf Map.empty inp [] $
      Map.fromList        $
        (\f -> (name f, f)) <$> functions

parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg prog inp =
  case runParser parseProg prog of
    Success (InputStream _ _) res -> evalProg res inp
    _                             -> Nothing