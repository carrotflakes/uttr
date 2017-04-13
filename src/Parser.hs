module Parser
     ( parseStatement,
       parseStatements
     ) where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Applicative hiding ((<|>), many)

import Ast


languageDef = emptyDef
  {
    P.commentStart = "/*",
    P.commentEnd = "*/",
    P.commentLine = "//",
    P.nestedComments = True,
    P.reservedNames = ["true", "false", "null"]
  }

lexer  = P.makeTokenParser languageDef
whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
brackets = P.brackets lexer
braces = P.braces lexer
identifier = T.pack <$> P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
integer = P.integer lexer
float = P.float lexer
stringLiteral = T.pack <$> P.stringLiteral lexer
comma = P.comma lexer
semi = P.semi lexer


data Mode = ExpressionMode | PatternMode deriving (Eq)


parseStatements :: String -> String -> Either ParseError [Statement]
parseStatements = parse $ do
  whiteSpace
  sts <- many statement
  eof
  return sts


parseStatement :: String -> String -> Either ParseError Statement
parseStatement = parse $ do
  st <- statement
  eof
  return st


statement = do
  expr <- (DefinitionStatement <$> try definition <|>
           ExpressionStatement <$> expression ExpressionMode)
  Text.Parsec.optional semi
  return expr

definition = try functionDefinition <|> constantDefinition

functionDefinition = do
  whiteSpace
  ident <- identifier
  whiteSpace
  block <- block
  return $ Definition ident $ ValueExpression $ FunctionValue ident block

constantDefinition = do
  whiteSpace
  ident <- identifier
  whiteSpace
  string "="
  whiteSpace
  expr <- expression ExpressionMode
  return $ Definition ident expr


patterns = parens (expressions PatternMode) <|> do
  pattern <- expression PatternMode
  return [pattern]

expression mode = operatorSystem mode
expressions mode = sepBy (expression mode) comma


operatorSystem mode = buildExpressionParser (operators mode) (applies mode)

operators mode =
  [ [ Prefix (reservedOp "-" >> return (\x -> apply "-" [x]))
    ]
  , [ Infix (reservedOp ":" >> return ConsExpression) AssocRight
    ]
  , [ infixOp "*" AssocLeft
    , infixOp "/" AssocLeft
    , infixOp "%" AssocLeft
    ]
  , [ infixOp "+" AssocLeft
    , infixOp "-" AssocLeft
    ]
  , [ infixOp "==" AssocNone
    , infixOp "!=" AssocNone
    , infixOp ">" AssocNone
    , infixOp "<" AssocNone
    , infixOp ">=" AssocNone
    , infixOp "<=" AssocNone
    ]
  , [ infixOp "&&" AssocNone
    , infixOp "||" AssocNone
    ]
  ]
  where
    infixOp name assoc =
      Infix (reservedOp name >> return (\x y -> apply name [x, y]))
      assoc
    apply op args = ApplyExpression (ValueExpression $ StringValue $ T.pack op) args

applies mode@ExpressionMode = factor mode >>= f
  where
    f left = do
      args <- parens (expressions mode)
      f (ApplyExpression left args)
     <|> do
      arg <- brackets (expression mode)
      f (ApplyExpression (ValueExpression $ StringValue $ T.pack "[]") [left, arg])
     <|> do
      P.dot lexer
      ident <- identifier
      f (ApplyExpression (ValueExpression $ StringValue $ T.pack"[]")
         [left, ValueExpression $ StringValue ident])
     <|> return left
applies mode = try (do
  ident <- identifier
  args <- parens (expressions mode)
  f (ApplyExpression (VariableExpression ident) args))
  <|> factor mode
  where
    f left = do
      args <- parens (expressions mode)
      f (ApplyExpression left args)
     <|> return left

factor mode@ExpressionMode
  = parens (operatorSystem mode)
  <|> ValueExpression <$> valueExpression
  <|> VariableExpression <$> identifier
  <|> ListExpression <$> try (brackets $ expressions mode)
  <|> ObjectExpression <$> braces (P.commaSep lexer $ member mode)
  <|> ClosureExpression <$> brackets block
factor mode@PatternMode
  = parens (operatorSystem mode)
  <|> ValueExpression <$> valueExpression
  <|> (ValueExpression . VariableValue) <$> identifier
  <|> ListExpression <$> try (brackets $ expressions mode)
  <|> ObjectExpression <$> braces (P.commaSep lexer $ member mode)

member mode
  = try (do
    ident <- identifier <|> stringLiteral
    P.colon lexer
    expr <- expression mode
    return (ident, expr))
  <|> do
    ident <- identifier
    return (ident, VariableExpression ident)

block = sepBy1
 (do
    patterns <- patterns
    guardClausesOrBody <- body <|> guardClauses
    whereClause <- (braces $ sepEndBy1 definition (Text.Parsec.optional semi)) <|> return []
    return (patterns, guardClausesOrBody, whereClause))
  comma

body = do
  whiteSpace
  char '='
  whiteSpace
  body <- expression ExpressionMode
  whiteSpace
  return $ Left body

guardClauses = Right <$> many1 guardClause

guardClause = do
  whiteSpace
  char '|'
  whiteSpace
  guard <- expression ExpressionMode
  whiteSpace
  char '='
  whiteSpace
  body <- expression ExpressionMode
  whiteSpace
  return (guard, body)


valueExpression
  =   NumberValue <$> try float
  <|> (NumberValue . fromIntegral) <$> integer
  <|> StringValue <$> stringLiteral
  <|> (reserved "true" >> return (BoolValue True))
  <|> (reserved "false" >> return (BoolValue False))
  <|> (reserved "null" >> return NullValue)
