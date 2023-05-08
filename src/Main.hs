{-# HLINT ignore "Use <$>" #-}

module Main (main) where

import Parser (parser)
import Types

import Control.Arrow ((>>>))
import Text.Megaparsec (errorBundlePretty, parse)
import Data.Map.Strict (Map, empty, insert, (!?))

main :: IO ()
main = do
    fileContents <- readFile "main.otlc"
    case parse parser "" fileContents of
        Left e -> putStr (errorBundlePretty e)
        Right (Definition name expr) ->
            case typeCheck empty expr of
                Right expr' -> print expr'
                Left err -> putStrLn err

-- print (Definition name (typeOf expr))

pipeline :: Expression -> Expression
pipeline =
    betaReduction
        >>> id

type Environment = Map Var Type


typeCheck :: Environment -> Expression -> Either String Type
typeCheck env (Variable v) =
    case env !? v of
        Just type' -> Right type'
        Nothing -> Left $ "Type-checking '" ++ show v ++ "' failed." 
typeCheck _ (Constant _) = Right $ T BaseTypeInt
typeCheck env (Abstraction v type' expr) =
    let newEnv = insert v type' env
    in (type' :->) <$> typeCheck newEnv expr
typeCheck env (Application expr1 expr2) = do
    typeOfExpr1 <- typeCheck env expr1
    typeOfExpr2 <- typeCheck env expr2
    case (typeOfExpr1, typeOfExpr2) of
        (a :-> b, c) ->
            if c == a
                then Right b
                else Left $ "Could not match '" ++ show c ++ "' with '" ++ show a ++ "'."
        (T _, _) -> Left $ "Value '" ++ show expr1 ++ "' is not a function."

betaReduction :: Expression -> Expression
betaReduction expr =
    let newExpr = reduce expr
     in if newExpr == expr
            then expr
            else betaReduction newExpr

reduce :: Expression -> Expression
reduce (Constant c) = Constant c
reduce (Application (Abstraction v _ expr1) expr2) = replacement v expr2 expr1
reduce (Application expr1 expr2) = Application (reduce expr1) (reduce expr2)
reduce (Variable var) = Variable var
reduce (Abstraction v type' expr) = Abstraction v type' (reduce expr)

-- TODO Renaming (to avoid clashes)
replacement :: Var -> Expression -> Expression -> Expression
replacement _ _ (Constant c) = Constant c
replacement var expr (Variable var')
    | var == var' = expr
    | otherwise = Variable var'
replacement var constant (Application e1 e2) =
    Application
        (replacement var constant e1)
        (replacement var constant e2)
replacement var constant (Abstraction var' type' expr) =
    Abstraction var' type' (replacement var constant expr)
