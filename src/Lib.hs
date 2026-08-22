module Lib
    ( someFunc
    ) where

import Data.Char (ord, chr)

data Term =
    Variable Char
  | Abstraction Char Term
  | Application Term Term 
  deriving(Show)

next :: Char -> Char
next 'a' = 'z'
next x = chr (ord x + 1)

collectUsed :: Term -> [Char]
collectUsed (Variable c) = [c] 
collectUsed (Abstraction c t) = [c] ++ collectUsed t
collectUsed (Application x y) = collectUsed x ++ collectUsed y


alphaConvert :: Term -> Char -> Term
alphaConvert (Abstraction old t) new = Abstraction new (rename t new old ([old] ++ collectUsed(t)))
alphaConvert term _ = term

rename :: Term -> Char -> Char -> [Char] -> Term
rename term new old used = term -- TODO implemented

someFunc :: IO ()
someFunc = print $ alphaConvert (Abstraction 'x' (Abstraction 'y' (Variable 'x'))) 'y'

