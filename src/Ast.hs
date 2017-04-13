module Ast
       where


import Data.List
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Text.IO as T


type Text = T.Text


type Env = Map Identifier Value

type Scope = ([(Identifier, Value)], Env)


type Identifier = Text

type Block = [([Expression], Either Expression [(Expression, Expression)], [Definition])]

data Statement
  = DefinitionStatement Definition
  | ExpressionStatement Expression
  deriving (Eq)

data Definition = Definition Identifier Expression
  deriving (Eq)

data Expression
  = ValueExpression Value
  | VariableExpression Identifier
  | ApplyExpression Expression [Expression]
  | ListExpression [Expression]
  | ObjectExpression [(Text, Expression)]
  | ConsExpression Expression Expression
  | TemplateLiteralExpression [Either Expression Text]
  | ClosureExpression Block
  deriving (Eq)

data Value
  = NumberValue Double
  | StringValue Text
  | BoolValue Bool
  | NullValue
  | ListValue [Value]
  | ObjectValue [(Text, Value)]
  | FunctionValue Identifier Block
  | ClosureValue Scope Block
  | VariableValue Identifier
  deriving (Eq)


instance Show Statement where
  show (DefinitionStatement definition) = show definition
  show (ExpressionStatement expression) = show expression

instance Show Definition where
  show (Definition ident (ValueExpression (FunctionValue _ block)))
    = (T.unpack ident) ++ " " ++ intercalate ", " (map showMC block)
    where
      showMC (patterns, Left body, whereClause) = showPatterns patterns ++ " = " ++ show body ++ showWC whereClause
      showMC (patterns, Right guardClauses, whereClause) = showPatterns patterns ++ concat (map showGC guardClauses) ++ showWC whereClause
      showGC (guard, body) = " | " ++ show guard ++ " = " ++ show body
      showWC [] = ""
      showWC defs = " {" ++ intercalate "; " (map show defs) ++ "}"
  show (Definition ident body)
    = (T.unpack ident) ++ " = " ++ show body

instance Show Expression where
  show (ValueExpression value) = show value
  show (VariableExpression ident) = (T.unpack ident)
  show (ApplyExpression func args)
    = shows func $ "(" ++ intercalate ", " (map show args) ++ ")"
  show (ListExpression elms) = "[" ++ intercalate ", " (map show elms) ++ "]"
  show (ObjectExpression membs)
    = "{" ++ intercalate ", " (map showMember membs) ++ "}"
    where showMember (key, value) = shows key $ ": " ++ show value
  show (ConsExpression car cdr) = shows car $ ":" ++ show cdr
  show (ClosureExpression block) = "[" ++ intercalate ", " (map showMC block) ++ "]"
    where
      showMC (patterns, Left body, whereClause) = showPatterns patterns ++ " = " ++ show body ++ showWC whereClause
      showMC (patterns, Right guardClauses, whereClause) = showPatterns patterns ++ concat (map showGC guardClauses) ++ showWC whereClause
      showGC (guard, body) = " | " ++ show guard ++ " = " ++ show body
      showWC [] = ""
      showWC defs = " {" ++ intercalate "; " (map show defs) ++ "}"

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
  show (FunctionValue ident _) = "<function " ++ (T.unpack ident) ++ ">"
  show (ClosureValue _ matchClauses) = "<closure>"
  show (VariableValue ident) = (T.unpack ident)

showPatterns [pattern] = show pattern
showPatterns patterns = "(" ++ intercalate ", " (map show patterns) ++ ")"
