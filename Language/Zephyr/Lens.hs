{-# LANGUAGE Rank2Types #-}
module Language.Zephyr.Lens where

import Control.Lens
import Control.Applicative
import Language.Zephyr.Syntax

tyvars :: Traversal' Type TyVar
tyvars f (VarT s) = VarT <$> f s
tyvars f (AppT p q) = AppT <$> tyvars f p <*> tyvars f q
tyvars _ t = pure t

tyvar :: TyVar -> Traversal' Type Type
tyvar s f (VarT v) | s == v = f (VarT v)
tyvar s f (AppT a b) = AppT <$> tyvar s f a <*> tyvar s f b
tyvar _ _ t = pure t

patVars :: Traversal' (Pat a) (Name, a)
patVars f (t :< VarP v) = f (v, t) <&> \(v', t') -> t' :< VarP v'
patVars f (t :< WildP) = pure (t :< WildP)
patVars f (t :< ConP n ss) = (t :<) <$> ConP n <$> traverse (patVars f) ss
patVars f (t :< SigP s p) = (t :<) <$> SigP s <$> patVars f p

var :: Name -> Traversal' (Expr a) (Expr a)
var name f (t :< VarE n) | name == n = f (t :< VarE n)
var name f (t :< AppE a b) = (t :<) <$> (AppE <$> var name f a <*> var name f b)
var name f (t :< LambdaE cs) = (t :<) <$> (LambdaE <$> traverse (traverse (var name f)) cs)
var name f (t :< LitE l) = pure (t :< LitE l)
var _ _ (t :< HoleE) = pure (t :< HoleE)