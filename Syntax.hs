{-# LANGUAGE ImplicitParams #-}
module Language.LazyZ.Syntax where

import Control.Applicative
import Text.Trifecta

data Dec = FunD String [([Pat], Exp)] | ValD Pat Exp

data Exp = VarE String
    | AppE Exp Exp
    | Lambda Pat Exp
    | Lit Lit
    deriving (Show, Eq)

data Lit = IntegerL Integer | StringL String

data Pat = WildP | VarP String

lit :: Parser Lit
lit = choice [IntegerL <$> natural, StringL <$> stringLiteral]

identifier = ident haskellIdents

parseOperator :: Monad m => Parser (Int, Operator Parser Exp)
parseOperator = do
	assoc <- parseAssoc
	i <- natural
	spaces
	name <- identifier
	return $ Infix (reservedOp name >>= \op x y -> VarE op `AppE` x `AppE` y) assoc
	where
		parseAssoc = choice [AssocLeft <$ symbol "infixl", AssocRight <$ symbol "infixr", AssocNone <$ symbol "infix"]

parseValD :: Given OperatorTable => Parser Dec
	lh <- parsePat
    symbol "="
    rh <- expr
    return $ ValD lh rh

parsePat :: Parser Pat
parsePat = choice [WildP <$ symbol "_", VarP <$> identifier]

lambda :: Given OperatorTable => Parser Exp
lambda = do
	symbol "λ" <|> symbol "\\"
	p <- parsePat
	symbol "->"
	e <- expr
	return $ Lambda p e

term :: Given OperatorTable => Parser Exp
term = whiteSpace *> term' <* whiteSpace where
    term' = try section
        <|> parens expr
        <|> lambda
        <|> try quote
        <|> literal
        <|> Var <$> identifier

termA :: Given OperatorTable => Parser Exp
termA = foldl Apply <$> term <*> many term

section :: Given OperatorTable => Parser Exp
section = Var <$> (char '(' *> operator <* char ')')

expr :: Given OperatorTable => Parser Exp
expr = buildExpressionParser given termA

