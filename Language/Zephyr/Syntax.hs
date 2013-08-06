{-# LANGUAGE FlexibleContexts, TemplateHaskell, Rank2Types, StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Language.Zephyr.Syntax (
    Name
    , ParseEnv(..)
    , Clause(..)
    , Dec(..)
    , ExprBase(..)
    , PatBase(..)
    , Expr
    , Expr'
    , arrT
    , exmap
    , Pat
    , Lit(..)
    , Type
    , TypeBase(..)
    , parseExpr
    , parseType
    , TyVar(..)
    , Kind(..)
    , module Control.Comonad.Cofree
    , def
    ) where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Lens
import Data.Foldable (Foldable(foldMap))
import Data.Monoid
import Data.Reflection
import Data.Traversable
import Text.Parser.Expression
import Text.Parser.Token.Style
import Text.Trifecta
import Data.Void
import Data.Default

type Name = String

data Clause b a = Clause [Pat b] a deriving (Show, Eq, Functor, Foldable, Traversable)

data Lit = IntegerL Integer | StringL String deriving (Show, Eq)

data Dec a = FunD Name [Clause a a] | ValD (Pat a) (Expr a a) deriving (Show, Eq)

data ExprBase b a = VarE Name
	| BoundE Int
    | SigE (Type ()) a
    | AppE a a
    | LambdaE [Clause b a]
    | LitE Lit
    | HoleE
    deriving (Show, Eq, Functor, Foldable, Traversable)

data PatBase a = WildP
    | VarP Name
    | ConP Name [a]
    | LitP Lit
    | SigP (Type ()) a deriving (Show, Eq, Functor, Foldable, Traversable)

type Pat = Cofree PatBase
type Expr p = Cofree (ExprBase p)

type Expr' a = Expr a a

exmap :: (a -> b) -> Cofree (ExprBase a) a -> Cofree (ExprBase b) b
exmap f (t :< LambdaE cs) = f t :< LambdaE (map go cs) where
    go (Clause ps a) = Clause (fmap (fmap f) ps) (exmap f a)
exmap f (t :< VarE n) = f t :< VarE n
exmap f (t :< SigE s e) = f t :< SigE s (exmap f e)
exmap f (t :< AppE a b) = f t :< AppE (exmap f a) (exmap f b)
exmap f (t :< LitE l) = f t :< LitE l
exmap f (t :< BoundE v) = f t :< BoundE v
exmap f (t :< HoleE) = f t :< HoleE

data TypeBase a = ArrT | VarT TyVar | ConT Name | AppT a a | ForallT TyVar Kind a | SigT Kind a deriving (Show, Eq, Functor, Foldable, Traversable)

data Kind = StarK | ConK Name | FunK Kind Kind deriving (Show, Eq, Ord)

type Type = Cofree TypeBase

data TyVar = AutoVar Int

sectionType :: Parser (Type ())
sectionType = (():<ArrT) <$ parens (symbol "->")

parseKind :: Parser Kind
parseKind = choice [StarK <$ symbol "*", FunK <$> parseKind <*> parseKind]

forallType :: Given ParseEnv => Parser (Type ())
forallType = do
    symbol "forall"
    v <- identifier
    k <- symbol "::" *> parseKind
    symbol "=>"
    t <- parseType
    return $ () :< ForallT (UserVar v) k t

parseType :: Given ParseEnv => Parser (Type ())
parseType = forallType <|> buildExpressionParser (typeOperators given) termTypeA
