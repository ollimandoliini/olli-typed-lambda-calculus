{-# HLINT ignore "Use <$>" #-}

module Main (main) where

import Control.Applicative (Alternative (empty, many), (<**>))
import Data.Char (isLetter)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, try), Parsec, between, errorBundlePretty, optional, parse, parseTest, satisfy, (<|>))
import Text.Megaparsec.Char (space, space1, string)
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

newtype Var = Var Char deriving (Eq)

instance Show Var where
    show :: Var -> String
    show (Var v) = [v]

newtype Const
    = ConstInt Int

instance Show Const where
    show :: Const -> String
    show (ConstInt i) = show i

data Expression
    = Variable Var
    | Abstraction Var (Maybe Type) Expression
    | Application Expression Expression
    | Constant Const

data Definition = Definition String Expression

instance Show Definition where
    show (Definition name expr) = name ++ " = " ++ show expr

instance Show Expression where
    show :: Expression -> String
    show (Variable c) = show c
    show (Abstraction c type' expr) =
        "(λ" ++ show c ++ maybe "" (\x -> " : " ++ show x) type' ++ " . " ++ show expr ++ ")"
    show (Constant c) = show c
    show (Application e1 e2) = show e1 ++ " " ++ show e2

data BaseType
    = BaseTypeInt
    deriving (Show)

data Type
    = T BaseType
    | Type :-> Type

instance Show Type where
    show :: Type -> String
    show (T BaseTypeInt) = "Int"
    show (type1 :-> type2) = show type1 ++ " -> " ++ show type2

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
    mType <- optional $ do
        symbol ":"
        pType
    symbol "."
    expr <- pExpr
    pure $ Abstraction var mType expr

pApplication :: Parser Expression
pApplication =
    chainl1
        ((Variable <$> pVar) <|> pConst <|> parens pExpr)
        (pure Application)

pExpr :: Parser Expression
pExpr = pAbstraction <|> pApplication

pDefinition :: Parser Definition
pDefinition = do
    name <- (lexeme . many . satisfy) isLetter
    symbol "="
    expr <- pExpr
    pure $ Definition name expr

fully :: Parser a -> Parser a
fully p = space *> p <* eof

parser :: Parser Expression
parser = fully pExpr

main :: IO ()
main = do
    fileContents <- readFile "test.otlc"
    case parse pDefinition "" fileContents of
        Left e -> putStr (errorBundlePretty e)
        Right (Definition name expr) -> print (Definition name (betaReduction expr))

betaReduction :: Expression -> Expression
betaReduction (Constant c) = Constant c
betaReduction (Application (Abstraction v _ expr) (Constant c)) =
    replacement v c expr
betaReduction _ = undefined

-- TODO Renaming (to avoid clashes)
-- TODO Only replaces constants for now
replacement :: Var -> Const -> Expression -> Expression
replacement _ _ (Constant c) = Constant c
replacement var constant (Variable var')
    | var == var' = Constant constant
    | otherwise = Variable var'
replacement var constant (Application e1 e2) =
    Application
        (replacement var constant e1)
        (replacement var constant e2)
replacement var constant (Abstraction var' type' expr) =
    Abstraction var' type' (replacement var constant expr)


foo :: Maybe Int -> Int
foo x = case x of
    