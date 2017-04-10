module Ast
       where


import Data.List
import Data.Map.Strict (Map)


type Env = Map Identifier Value

type Scope = ([(Identifier, Value)], Env)


type Identifier = String

data Statement
  = DefinitionStatement Definition
  | ExpressionStatement Expression
  deriving (Eq)

data Definition
  = FunctionDefinition Identifier [Expression] Expression
  | ConstantDefinition Identifier Expression
  deriving (Eq)

type Operator = String

data Expression
  = ValueExpression Value
  | VariableExpression Identifier
  | ApplyExpression Expression [Expression]
  | ListExpression [Expression]
  | ObjectExpression [(String, Expression)]
  deriving (Eq)
--  | TemplateLitteral

data Value
  = NumberValue Double
  | StringValue String
  | BoolValue Bool
  | NilValue
  | ListValue [Value]
  | ObjectValue [(String, Value)]
  | FunctionValue Identifier [([Expression], Expression)]
  | ClosureValue Env [Expression] Expression
  deriving (Eq)


instance Show Statement where
  show (DefinitionStatement definition) = show definition
  show (ExpressionStatement expression) = show expression

instance Show Definition where
  show (FunctionDefinition ident params body)
    = ident ++ "(" ++ intercalate ", " (map show params) ++ ") = " ++ show body
  show (ConstantDefinition ident body)
    = ident ++ show body

instance Show Expression where
  show (ValueExpression value) = show value
  show (VariableExpression ident) = ident
  show (ApplyExpression func args)
    = shows func $ "(" ++ intercalate ", " (map show args) ++ ")"
  show (ListExpression elms) = "[" ++ intercalate ", " (map show elms) ++ "]"
  show (ObjectExpression membs)
    = "{" ++ intercalate ", " (map showMember membs) ++ "}"
    where showMember (key, value) = shows key $ ": " ++ show value

instance Show Value where
  show (NumberValue value)
    | fromIntegral (round value) == value = show $ round value
    | otherwise = show value
  show (StringValue value) = show value
  show (BoolValue True) = "t"
  show (BoolValue False) = "f"
  show NilValue = "nil"
  show (ListValue elms) = "[" ++ intercalate ", " (map show elms) ++ "]"
  show (ObjectValue membs) = "{" ++ intercalate ", " (map showMember membs) ++ "}"
    where showMember (key, value) = shows key $ ": " ++ show value
  show (FunctionValue ident _) = "<" ++ ident ++ ">"
  show (ClosureValue _ params body)
    = "<(" ++ intercalate ", " (map show params) ++ ") => " ++ shows body ">"
