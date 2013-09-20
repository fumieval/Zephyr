{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell #-}
module Language.Zephyr.Internal.Combinator where

import Prelude hiding (elem, notElem)
import Control.Monad.Free
import Data.Foldable
import Control.Lens

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Void

data SKI a = S | K | I | App a a | Undefined String deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

class SKILike f where
    _S :: f a
    _K :: f a
    _I :: f a
    _App :: Prism' (f a) (a, a)
    isPrimitive :: f a -> Bool

showUnlambda :: Free SKI String -> String
showUnlambda (Free I) = "i"
showUnlambda (Free K) = "k"
showUnlambda (Free S) = "s"
showUnlambda (Pure x) = "[" ++ x ++ "]"
showUnlambda (Free (App a b)) = "`" ++ showUnlambda a ++ showUnlambda b

showCC :: Bool -> Free SKI String -> String
showCC _ (Free I) = "I"
showCC _ (Free K) = "K"
showCC _ (Free S) = "S"
showCC _ (Pure x) = "[" ++ x ++ "]"
showCC False (Free (App a b)) = showCC False a ++ showCC True b
showCC True (Free (App a b)) = "(" ++ showCC False a ++ showCC True b ++ ")"

($$) :: SKILike f => Free f a -> Free f a -> Free f a
($$) f g = Free (_App # (f, g))

unApp :: SKILike f => Free f a -> Maybe (Free f a, Free f a)
unApp (Free f) = f ^? _App
unApp _ = Nothing

instance SKILike SKI where
    _S = S
    _K = K
    _I = I
    _App = prism' (\(a, b) -> App a b) (\r -> case r of
        App a b -> Just (a, b)
        _ -> Nothing)
    isPrimitive (App _ _) = False
    isPrimitive _ = True 

instance (Functor f, SKILike f) => SKILike (Free f) where
    _S = Free _S
    _K = Free _K
    _I = Free _I
    _App = prism' (\(a, b) -> Pure a $$ Pure b) go where
        go (Pure _) = Nothing
        go (Free f) = case f ^? _App of
            Just (Pure a, Pure b) -> Just (a, b)
            _ -> Nothing
    isPrimitive (Pure a) = True
    isPrimitive (Free f) = isPrimitive f

-- | The 'bindee v' function transforms an expression to a combinator which 'v' when applied.
bindee :: (Eq v, Eq (f (Free f v)), SKILike f, Functor f, Foldable f) => v -> Free f v -> Free f v
-- refered to John Tromp "Binary Lambda Calculus and Combinatory Logic", 2011 (http://homepages.cwi.nl/~tromp/cl/LC.pdf section 3.2)
-- bindee _ (S $$ K $$ _) = S $$ K
bindee x f              | x `notElem` f = _K $$ f
bindee x (Pure x')      | x == x' = _I
bindee x ex = go (unApp ex) where
    go (Just (al, ar))
        | Pure x' <- ar, x `notElem` al, x == x' = al

        | Just (Pure y, f) <- unApp al, Pure z <- ar
        , x == y, x == z
        = bindee x $ _S $$ _S $$ _K $$ Pure x $$ f

        | Just (g, h) <- unApp ar
        , isPrimitive al
        , isPrimitive g
        = bindee x $ _S $$ bindee x al $$ g $$ h

        | Just (f, g) <- unApp al
        , isPrimitive f
        , isPrimitive ar
        = bindee x $ _S $$ f $$ bindee x ar $$ g

        | Just (f, g) <- unApp al
        , Just (h, g') <- unApp ar
        , isPrimitive f
        , isPrimitive h
        , g == g'
        = bindee x $ _S $$ f $$ h $$ g

        | otherwise = _S $$ bindee x al $$ bindee x ar


unlambdaParser :: Read e => Parser (Expr e)
unlambdaParser = char '`' *> ((:$) <$> unlambdaParser <*> unlambdaParser)
    <|> char 's' *> pure S
    <|> char 'k' *> pure K
    <|> char 'i' *> pure I
    <|> Free <$> (char '[' *> some (satisfy (/=']')) <* char ']')
    <|> Extern <$> read <$> (char '<' *> some (satisfy (/='>')) <* char '>')

ccParser :: Read e => Parser (Expr e)
ccParser = foldl (:$) <$> term <*> many term where
    term = char '(' *> ccParser <* char ')'
        <|> char 'S' *> pure S
        <|> char 'K' *> pure K
        <|> char 'I' *> pure I
        <|> Free <$> (char '[' *> some (satisfy (/=']')) <* char ']')
        <|> Extern <$> read <$> (char '<' *> some (satisfy (/='>')) <* char '>')

toExp :: Expr Void -> ExpQ
toExp S = [|S|]
toExp K = [|K|]
toExp I = [|I|]
toExp (Free v) = [|Free v|]
toExp (f :$ g) = uInfixE (parensE $ toExp f) (conE '(:$)) (parensE $ toExp g)

cc :: QuasiQuoter
cc = QuasiQuoter { quoteExp = either (fail.show) toExp . parse ccParser "" }