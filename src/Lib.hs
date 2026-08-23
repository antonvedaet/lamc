{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Lib
    ( someFunc
    ) where

import Data.Char (ord, chr)

data Term =
    Variable    Char
  | Abstraction Char Term
  | Application Term Term 
  deriving(Show)
  
data BetaRedex = BetaRedex
    { boundVar :: Char
    , body     :: Term
    , argument :: Term
    }

asRedex :: Term -> Maybe BetaRedex
asRedex (Application (Abstraction v b) a) = Just (BetaRedex v b a)
asRedex _                                 = Nothing

causesOvertake :: BetaRedex -> Bool
causesOvertake (BetaRedex var b arg) =
    check b
    where
        check (Variable _) = False
        check (Application m n) = check m || check n
        check (Abstraction c t) = True -- TODO

next :: Char -> Char
next 'a' = 'z'
next x = chr (ord x + 1)

collectUsed :: Term -> [Char]
collectUsed (Variable c)       = [c] 
collectUsed (Abstraction c t)  = [c] ++ collectUsed t
collectUsed (Application m n) = collectUsed m ++ collectUsed n

rename :: Term -> Char -> Char -> [Char] -> Term
rename term new old used = term -- TODO

alphaConvert :: Term -> Char -> Term
alphaConvert (Abstraction old t) new = Abstraction new (rename t new old ([old] ++ collectUsed(t)))
alphaConvert term _                  = term

betaReduction :: p
betaReduction = betaReduction -- TODO

someFunc :: IO ()
someFunc = print $ alphaConvert (Abstraction 'x' (Abstraction 'y' (Variable 'x'))) 'y'

