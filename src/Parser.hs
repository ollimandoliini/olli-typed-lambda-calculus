module Parser (parser) where

import Types

import Control.Applicative
import Data.Char (isLetter)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, try), Parsec, between, satisfy)
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer =
    L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

token :: Parser a -> Parser a
token = lexeme . try

chainl1 :: Parser b -> Parser (b -> b -> b) -> Parser b
chainl1 = infixl1 id

infixl1 :: (a -> b) -> Parser a -> Parser (b -> a -> b) -> Parser b
infixl1 wrap p op = (wrap <$> p) <**> rest
  where
    rest = flip (.) <$> (flip <$> op <*> p) <*> rest <|> pure id

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pBaseType :: Parser BaseType
pBaseType = token (BaseTypeInt <$ string "Int")

pType :: Parser Type
pType = token $ do
    type' <- T <$> pBaseType
    let pTypes = do
            symbol "->"
            rest <- pType
            pure (type' :-> rest)
    pTypes <|> pure type'

pConst :: Parser Expression
pConst = token $ Constant . ConstInt <$> L.decimal

pVar :: Parser Var
pVar = token $ Var <$> satisfy isLetter

pAbstraction :: Parser Expression
pAbstraction = do
    symbol "λ"
    var <- pVar
    mType <- symbol ":" *> pType
    symbol "."
    Abstraction var mType <$> pExpr

pApplication :: Parser Expression
pApplication =
    chainl1
        ((Variable <$> pVar) <|> pConst <|> parens pExpr)
        (pure Application)

pExpr :: Parser Expression
pExpr = pAbstraction <|> pApplication

pDefinition :: Parser Definition
pDefinition = do
    Definition <$> (name <* symbol "=") <*> pExpr
  where
    name = (lexeme . many . satisfy) isLetter

fully :: Parser a -> Parser a
fully p = spaceConsumer *> p <* eof

parser :: Parser Definition
parser = fully pDefinition