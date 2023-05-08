module Types where

newtype Var = Var Char deriving (Eq, Ord)

instance Show Var where
    show :: Var -> String
    show (Var v) = [v]

newtype Const
    = ConstInt Int
    deriving (Eq)

instance Show Const where
    show :: Const -> String
    show (ConstInt i) = show i

data Expression
    = Variable Var
    | Abstraction Var Type Expression
    | Application Expression Expression
    | Constant Const
    deriving (Eq)

data Definition = Definition String Expression

instance Show Definition where
    show (Definition name expr) = name ++ " = " ++ show expr

instance Show Expression where
    show :: Expression -> String
    show (Variable c) = show c
    show (Abstraction c type' expr) =
        "(λ" ++ show c ++ " : " ++ show type' ++ " . " ++ show expr ++ ")"
    show (Constant c) = show c
    show (Application e1 e2) = show e1 ++ " " ++ show e2

data BaseType
    = BaseTypeInt
    deriving (Show, Eq)

data Type
    = T BaseType
    | Type :-> Type
    deriving (Eq)

instance Show Type where
    show :: Type -> String
    show (T BaseTypeInt) = "Int"
    show (type1 :-> type2) = show type1 ++ " -> " ++ show type2