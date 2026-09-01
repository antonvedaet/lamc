module Lambda.Term
    ( Term (..)
    , BetaRedex (..)
    ) where

data Term
    = Variable Char
    | Abstraction Char Term
    | Application Term Term
    deriving (Show, Eq)

data BetaRedex = BetaRedex
    { boundVar :: Char
    , body :: Term
    , argument :: Term
    }