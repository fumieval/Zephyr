{-# LANGUAGE Rank2Types #-}
module Language.Zephyr.TypeCheck where
import Language.Zephyr.Syntax
import Control.Lens
import Control.Applicative
import Control.Comonad.Cofree

type Name = String

data ExprBase a = Var String | App a a | Lambda Pat a | Lit Lit deriving (Show, Eq)

type Expr = Cofree ExprBase Type

data Type = ArrT | VarT Name | ConT Name | AppT Type Type deriving (Show, Eq)

type Subst = (Name, Type)

vars :: Traversal' Type Name
vars f (VarT s) = VarT <$> f s
vars f (AppT p q) = AppT <$> vars f p <*> vars f q

varBind :: Monad m => Name -> Type -> m [Subst]
varBind v t
    | t == VarT v = return []
    | elemOf vars v t = fail "occurs check fails"
    | otherwise = return [(v, t)]

var :: Name -> Traversal' Type Type
var s f (VarT v) | s == v = f (VarT v)
var s f (AppT a b) = AppT <$> var s f a <*> var s f b

merge :: [Subst] -> [Subst] -> [Subst]
merge s t = (traverse . _2 %~ apply s) t ++ s

apply :: [Subst] -> Type -> Type
apply = flip $ foldr (\(s, r) -> var s .~ r)

unify :: Monad m => Type -> Type -> m [Subst]
unify (AppT s t) (AppT u v) = do
    a <- unify s u
    b <- unify (apply a t) (apply a v)
    return (merge a b)
unify (VarT s) t = varBind s t
unify s (VarT t) = varBind t s
unify (ConT s) (ConT t) | s == t = return []
unify _ _ = fail "Failed to unify"

