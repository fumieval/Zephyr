import Control.Lens
import Control.Applicative

type Name = String

data ExprBase a = Var String | App a a | Lambda | Lit Lit deriving (Show, Eq)

type Expr = Cofree ExprBase Type deriving (Show, Eq)

data Type = ArrT | VarT Name | ConT Name | AppT Type Type deriving (Show, Eq)

data Kind = Star | FunK Kind Kind deriving (Show, Eq)

type Substitution = (String, Type)

_VarT :: Prism' Type Name
_VarT = prism' VarT go where
    go (VarT s) = Just s
    go _ = Nothing

vars :: Traversal' Type Name
vars f (VarT s) = VarT <$> f s
vars f (AppT p q) = AppT <$> vars f p <*> vars f q

kind :: Type -> Kind
kind _ = Star

varBind :: Name -> Type -> Substitution
varBind u t
    | t == VarT u = return []
    | elemOf vars t = fail "occurs check fails"
    | kind u /= kind t = fail "kinds do not match"
    | otherwise = return (u, t)

unify :: Monad m => Type -> Type -> m Substitution
unify (VarT s) t = varBind s t
unify s (VarT t) = varBind t s
unify (ConT s) (ConT t) | s == t = return []
unify _ _ = fail "Failed to unify"

typeCheck :: Exp -> Expr
