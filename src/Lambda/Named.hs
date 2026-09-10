module Lambda.Named
    ( asRedex
    , alphaConvert
    , betaReduction
    ) where

import Data.Char (chr, ord)
import Data.Function (fix)
import Lambda.Term (BetaRedex (..), Term (..))

asRedex :: Term -> Maybe BetaRedex
asRedex (Application (Abstraction v b) a) = Just (BetaRedex v b a)
asRedex _ = Nothing

next :: Char -> Char
next 'z' = 'a'
next x = chr (ord x + 1)

collectUsed :: Term -> [Char]
collectUsed (Variable c) = [c]
collectUsed (Abstraction c t) = c : collectUsed t
collectUsed (Application left right) = collectUsed left ++ collectUsed right

rename :: Term -> Char -> Char -> [Char] -> Term
rename (Application left right) new old used = Application (rename left new old used) (rename right new old used)
rename (Variable v) new old _
    | v == old = Variable new
    | otherwise = Variable v
rename (Abstraction binder term) new old used
    | binder == old = Abstraction binder term
    | binder == new =
        let fresh = fix (\f n -> if n `notElem` used then n else f $ next n) 'a'
            inner = rename term fresh binder (fresh : used)
         in Abstraction fresh (rename inner new old (fresh : used))
    | otherwise = Abstraction binder (rename term new old used)

alphaConvert :: Term -> Char -> Maybe Term -- FIX: Free var capture
alphaConvert abst@(Abstraction old t) new
    | new `elem` freeVars abst = Nothing
    | otherwise = Just $ Abstraction new (rename t new old (new : old : collectUsed t))
alphaConvert term _ = Just $ term

freeVars :: Term -> [Char]
freeVars (Variable c) = [c]
freeVars (Abstraction c t) = (filter (/= c) (freeVars t))
freeVars (Application left right) = (freeVars left) ++ (freeVars right)

substitute :: Char -> Term -> Term -> Term
substitute v (Variable b) a
    | v == b = a
    | otherwise = Variable b
substitute v (Application left right) a = (Application (substitute v left a) (substitute v right a))
substitute v abst@(Abstraction binder inner) a
    | v == binder = Abstraction binder inner
    | binder `elem` (freeVars a) =
        let used = collectUsed inner ++ collectUsed a ++ [v, binder]
            fresh = fix (\f n -> if n `notElem` used then n else f $ next n) binder
            renamed = alphaConvert abst fresh
         in case renamed of
                Just (Abstraction fresh' inner') -> Abstraction fresh' (substitute v inner' a)
                _ -> error "alphaConvert returned a non-abstraction"
    | otherwise = Abstraction binder (substitute v inner a)

betaReduction :: BetaRedex -> Term
betaReduction (BetaRedex v b a) = substitute v b a
