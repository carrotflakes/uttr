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
  = FunctionDefinition Identifier [([Expression], Either Expression [(Expression, Expression)])]
  | ConstantDefinition Identifier Expression
  deriving (Eq)

type Operator = String

data Expression
  = ValueExpression Value
  | VariableExpression Identifier
  | ApplyExpression Expression [Expression]
  | ListExpression [Expression]
  | ObjectExpression [(String, Expression)]
  | ConsExpression Expression Expression
  | ClosureExpression [([Expression], Either Expression [(Expression, Expression)])]
  deriving (Eq)
--  | TemplateLitteral

data Value
  = NumberValue Double
  | StringValue String
  | BoolValue Bool
  | NullValue
  | ListValue [Value]
  | ObjectValue [(String, Value)]
  | FunctionValue Identifier [([Expression], Either Expression [(Expression, Expression)])]
  | ClosureValue Scope [([Expression], Either Expression [(Expression, Expression)])]
  | VariableValue Identifier
  deriving (Eq)


instance Show Statement where
  show (DefinitionStatement definition) = show definition
  show (ExpressionStatement expression) = show expression

instance Show Definition where
  show (FunctionDefinition ident matchClauses)
    = ident ++ " " ++ intercalate ", " (map showMC matchClauses)
    where
      showMC (patterns, Left body) = showPatterns patterns ++ " = " ++ show body
      showMC (patterns, Right guardClauses) = showPatterns patterns ++ concat (map showGC guardClauses)
      showGC (guard, body) = " | " ++ show guard ++ " = " ++ show body
  show (ConstantDefinition ident body)
    = ident ++ " = " ++ show body

instance Show Expression where
  show (ValueExpression value) = show value
  show (VariableExpression ident) = ident
  show (ApplyExpression func args)
    = shows func $ "(" ++ intercalate ", " (map show args) ++ ")"
  show (ListExpression elms) = "[" ++ intercalate ", " (map show elms) ++ "]"
  show (ObjectExpression membs)
    = "{" ++ intercalate ", " (map showMember membs) ++ "}"
    where showMember (key, value) = shows key $ ": " ++ show value
  show (ConsExpression car cdr) = shows car $ ":" ++ show cdr
  show (ClosureExpression matchClauses) = "[" ++ intercalate ", " (map showMC matchClauses) ++ "]"
    where
      showMC (patterns, Left body) = showPatterns patterns ++ " = " ++ show body
      showMC (patterns, Right guardClauses) = showPatterns patterns ++ concat (map showGC guardClauses)
      showGC (guard, body) = " | " ++ show guard ++ " = " ++ show body

instance Show Value where
  show (NumberValue value)
    | fromIntegral (round value) == value = show $ round value
    | otherwise = show value
  show (StringValue value) = show value
  show (BoolValue True) = "true"
  show (BoolValue False) = "false"
  show NullValue = "null"
  show (ListValue elms) = "[" ++ intercalate ", " (map show elms) ++ "]"
  show (ObjectValue membs) = "{" ++ intercalate ", " (map showMember membs) ++ "}"
    where showMember (key, value) = shows key $ ": " ++ show value
  show (FunctionValue ident _) = "<function " ++ ident ++ ">"
  show (ClosureValue _ matchClauses) = "<closure>"
  show (VariableValue ident) = ident

showPatterns [pattern] = show pattern
showPatterns patterns = "(" ++ intercalate ", " (map show patterns) ++ ")"
