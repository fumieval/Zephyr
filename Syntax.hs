{-# LANGUAGE FlexibleContexts #-}
module Language.Zephyr.Syntax where

import Control.Applicative
import Text.Trifecta
import Text.Parser.Expression
import Text.Parser.Token.Style
import Data.Reflection

data Clause = Clause [Pat] Exp deriving (Show, Eq)

data Dec = FunD String [Clause] | ValD Pat Exp deriving (Show, Eq)

data Exp = VarE String
    | AppE Exp Exp
    | LambdaE [Clause]
    | LitE Lit
    deriving (Show, Eq)

data Lit = IntegerL Integer | StringL String deriving (Show, Eq)

data Pat = WildP | VarP String deriving (Show, Eq)

type OpTable = OperatorTable Parser Exp

literal :: Parser Lit
literal = choice [IntegerL <$> natural, StringL <$> stringLiteral]

identifier = ident haskellIdents

parseOperator :: Parser (Int, Operator Parser Exp)
parseOperator = do
	assoc <- parseAssoc
	i <- natural
	spaces
	name <- identifier
	return (fromEnum i, Infix (symbol name >>= \op -> return $ \x y -> VarE op `AppE` x `AppE` y) assoc)
	where
		parseAssoc = choice [AssocLeft <$ symbol "infixl", AssocRight <$ symbol "infixr", AssocNone <$ symbol "infix"]

parseValD :: Given OpTable => Parser Dec
parseValD = do
    lh <- parsePat
    symbol "="
    rh <- expr
    return $ ValD lh rh

parsePat :: Parser Pat
parsePat = choice [WildP <$ symbol "_", VarP <$> identifier]

lambda :: Given OpTable => Parser Exp
lambda = do
	symbol "λ" <|> symbol "\\"
	ps <- some parsePat
	symbol "->"
	e <- expr
	return $ LambdaE [Clause ps e]

term :: Given OpTable => Parser Exp
term = token $ try section
        <|> parens expr
        <|> lambda
        <|> LitE <$> literal
        <|> VarE <$> identifier

termA :: Given OpTable => Parser Exp
termA = foldl AppE <$> term <*> many term

section :: Given OpTable => Parser Exp
section = empty -- = VarE <$> (char '(' *> operator <* char ')')

expr :: Given OpTable => Parser Exp
expr = buildExpressionParser given termA
