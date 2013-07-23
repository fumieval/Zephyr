{-# LANGUAGE Rank2Types #-}
module Language.Zephyr.Lens where

import Control.Lens
import Control.Applicative
import Language.Zephyr.Syntax
import Control.Comonad

tyvars :: Traversal' (Type k) TyVar
tyvars f = _unwrap go where
    go (VarT s) = VarT <$> f s
    go (AppT p q) = AppT <$> tyvars f p <*> tyvars f q
    go t = pure t
{-# INLINE tyvars #-}

tyvar :: TyVar -> Traversal' (Type k) (Type k)
tyvar s f (t :< VarT v) | s == v = f (t :< VarT v)
tyvar s f (t :< AppT a b) = fmap (t :<) $ AppT <$> tyvar s f a <*> tyvar s f b
tyvar s f t = pure t
{-# INLINE tyvar #-}

patVars :: Traversal' (Pat a) (Name, a)
patVars f (t :< VarP v) = f (v, t) <&> \(v', t') -> t' :< VarP v'
patVars f (t :< WildP) = pure (t :< WildP)
patVars f (t :< ConP n ss) = (t :<) <$> ConP n <$> traverse (patVars f) ss
patVars f (t :< SigP s p) = (t :<) <$> SigP s <$> patVars f p
patVars f (t :< LitP l) = pure (t :< LitP l)

var :: Name -> Traversal' (Expr b a) (Expr b a)
var name f (t :< VarE n) | name == n = f (t :< VarE n)
var name f (t :< AppE a b) = (t :<) <$> (AppE <$> var name f a <*> var name f b)
var name f (t :< LambdaE cs) = (t :<) <$> (LambdaE <$> traverse (traverse (var name f)) cs)
var name f (t :< LitE l) = pure (t :< LitE l)
var name f (t :< SigE s l) = (t :<) <$> SigE s <$> var name f l
var name f (t :< BoundE v) = pure (t :< BoundE v)
var _ _ (t :< HoleE) = pure (t :< HoleE)
