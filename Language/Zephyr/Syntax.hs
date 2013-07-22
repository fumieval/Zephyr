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
    , exmap
    , Pat
    , Lit(..)
    , Type
    , TypeBase(..)
    , parseExpr
    , parseType
    , TyVar(..)
    , Kind(..)
    , Predicate(..)
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

exmap :: (a -> b) -> Cofree (ExprBase a) a -> Cofree (ExprBase b) b
exmap f (t :< LambdaE cs) = f t :< LambdaE (map go cs) where
    go (Clause ps a) = Clause (fmap (fmap f) ps) (exmap f a)
exmap f (t :< VarE n) = f t :< VarE n
exmap f (t :< SigE s e) = f t :< SigE s (exmap f e)
exmap f (t :< AppE a b) = f t :< AppE (exmap f a) (exmap f b)
exmap f (t :< LitE l) = f t :< LitE l
exmap f (t :< HoleE) = f t :< HoleE

type ExprTable = OperatorTable Parser (Expr () ())

data TypeBase a = ArrT | VarT TyVar | ConT Name | AppT a a | ForallT TyVar Kind a | SigT Kind a deriving (Show, Eq, Functor, Foldable, Traversable)

data Predicate = Dummy Predicate

deriving instance Show Predicate
deriving instance Eq Predicate
deriving instance Ord Predicate

data Kind = StarK | ConK Name | FunK Kind Kind deriving (Show, Eq, Ord)

type Type = Cofree TypeBase

data TyVar = AutoVar Int | UserVar String deriving (Show, Eq, Ord)

data ParseEnv = ParseEnv 
    { exprOperators :: OperatorTable Parser (Expr () ())
    , typeOperators :: OperatorTable Parser (Type ())
    }

instance Default ParseEnv where
    def = ParseEnv { exprOperators = []
        , typeOperators = [[Infix arr AssocRight]] } where
        arr = (\x y -> () :< AppT (() :< AppT (() :< ArrT) x) y) <$ symbol "->"

literal :: Parser Lit
literal = choice [IntegerL <$> natural, StringL <$> stringLiteral]

identifier :: Parser Name
identifier = ident haskellIdents

upperIdentifier :: Parser Name
upperIdentifier = (:) <$> upper <*> identifier

varE :: Name -> Expr () ()
varE n = () :< VarE n

litE :: Lit -> Expr () ()
litE l = () :< LitE l

appE :: Expr () () -> Expr () () -> Expr () ()
appE f g = () :< AppE f g

parseOperator :: Parser (Int, Operator Parser (Expr () ()))
parseOperator = do
	assoc <- parseAssoc
	i <- natural
	spaces
	name <- identifier
	return (fromEnum i, Infix (symbol name >>= \op -> return $ \x y -> varE op `appE` x `appE` y) assoc)
	where
		parseAssoc = choice [ AssocLeft <$ symbol "infixl"
                            , AssocRight <$ symbol "infixr"
                            , AssocNone <$ symbol "infix" ]

parseValD :: Given ParseEnv => Parser (Dec ())
parseValD = do
    lh <- parsePat
    symbol "="
    rh <- parseExpr
    return $ ValD lh rh

parsePat :: Given ParseEnv => Parser (Pat ())
parsePat = parens parsePat <|> body where
    body = do
        p <- choice [(() :< WildP) <$ symbol "_"
            , (():<) <$> (ConP <$> upperIdentifier <*> many parsePat)
            , (():<) <$> VarP <$> identifier
            , (():<) <$> LitP <$> literal ]
        sig <- optional $ symbol "::" *> parseType
        case sig of
            Nothing -> return p
            Just t -> return $ () :< SigP t p

lambda :: Given ParseEnv => Parser (Expr () ())
lambda = do
	symbol "λ" <|> symbol "\\"
	ps <- some parsePat
	symbol "->"
	e <- parseExpr
	return $ () :< LambdaE [Clause ps e]

term :: Given ParseEnv => Parser (Expr () ())
term = token $ try sectionOp
        <|> parens parseExpr
        <|> lambda
        <|> (() :< HoleE) <$ symbol "_"
        <|> litE <$> literal
        <|> varE <$> identifier

termA :: Given ParseEnv => Parser (Expr () ())
termA = foldl appE <$> term <*> many term

sectionOp :: Given ParseEnv => Parser (Expr () ())
sectionOp = empty -- = VarE <$> (char '(' *> operator <* char ')')

parseExpr :: Given ParseEnv => Parser (Expr () ())
parseExpr = do
    e <- buildExpressionParser (exprOperators given) termA
    sig <- optional $ symbol "::" *> parseType
    case sig of
        Nothing -> return e
        Just t -> return (() :< SigE t e)

termType :: Given ParseEnv => Parser (Type ())
termType = token $ try sectionType
    <|> parens parseType
    <|> (():<) <$> ConT <$> upperIdentifier
    <|> (():<) <$> VarT <$> UserVar <$> identifier

termTypeA :: Given ParseEnv => Parser (Type ())
termTypeA = foldl appT <$> termType <*> many termType where
    appT s t = () :< AppT s t

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