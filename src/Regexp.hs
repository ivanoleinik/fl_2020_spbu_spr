module Regexp where

import Prelude hiding (seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Show, Eq, Ord)

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

derivative :: Char -> Regexp -> Regexp
derivative _ Empty = Empty
derivative _ Epsilon = Empty
derivative x (Char c) =
  case x == c of
    True -> Epsilon
    _    -> Empty
derivative x (Seq l r) =
  case nullable l of
    True -> Alt (Seq (derivative x l) r) (derivative x r)
    _    -> Seq (derivative x l) r
derivative x (Alt l r) = Alt (derivative x l) (derivative x r)
derivative x (Star y)  = Seq (derivative x y) (Star y)

nullable :: Regexp -> Bool
nullable Empty = False
nullable Epsilon = True
nullable (Char _) = False
nullable (Seq l r) = nullable l && nullable r
nullable (Alt l r) = nullable l || nullable r
nullable (Star _) = True