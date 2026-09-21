module Lambda.Eval () where

import Lambda.Named (asRedex, betaReduction)
import Lambda.Term (Term (..))

step :: Term -> Maybe Term
step t =
    case asRedex t of
        Just redex -> Just (betaReduction redex)
        Nothing -> eval t
    where
        eval :: Term -> Maybe Term
        eval (Variable c) = Nothing
        eval (Application left right) = Just left -- TODO
        eval (Abstraction _ term) = step term
