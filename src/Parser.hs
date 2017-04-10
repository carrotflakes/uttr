module Parser
     ( parseStatement
     ) where

--import Text.Parsec.Prim
--import Text.Parsec.Combinator
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

import Control.Applicative hiding ((<|>), many)

import Ast


languageDef = emptyDef
  {
    P.reservedNames = ["t", "f", "nil", "case", "of"]
  }

lexer  = P.makeTokenParser languageDef
whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
brackets = P.brackets lexer
braces = P.braces lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
integer = P.integer lexer
float = P.float lexer
stringLiteral = P.stringLiteral lexer


parseStatement :: String -> String -> Either ParseError Statement
parseStatement = parse $ do
  st <- statement
  eof
  return st


statement =
    try (DefinitionStatement <$> definition)
  <|>
    ExpressionStatement <$> expression

definition = try functionDefinition <|> constantDefinition

functionDefinition = do
  ident <- identifier
  params <- parens expressions
  -- guard
  whiteSpace
  string "="
  whiteSpace
  expr <- expression
  return $ FunctionDefinition ident params expr

constantDefinition = do
  ident <- identifier
  whiteSpace
  string "="
  whiteSpace
  expr <- expression
  return $ ConstantDefinition ident expr

expressions = sepBy expression $ do
  whiteSpace
  char ','
  whiteSpace


expression = operatorSystem

valueExpression = do
    float <- try float
    return $ NumberValue $ float
  <|> do
    integer <- integer
    return $ NumberValue $ fromIntegral integer
  <|> do
    str <- stringLiteral
    return $ StringValue str
  <|>
    (reserved "t" >> return (BoolValue True))
  <|>
    (reserved "f" >> return (BoolValue False))
  <|>
    (reserved "nil" >> return NilValue)


operatorSystem = buildExpressionParser operators factor

operators =
  [ [
      Postfix $ do
        args <- parens expressions
        return $ \x -> ApplyExpression x args
    , Postfix $ do
        arg <- brackets expression
        return $ \x -> ApplyExpression (ValueExpression $ StringValue "[]") [x, arg]
    , Postfix $ do
        ident <- identifier
        return $ \x -> ApplyExpression (ValueExpression $ StringValue "[]") [x, ValueExpression $ StringValue ident]
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
      Infix(do
               reservedOp name
               return $ \x y -> ApplyExpression (ValueExpression $ StringValue name) [x, y])
      assoc

factor = parens operatorSystem
    <|> do
      val <- valueExpression
      return $ ValueExpression val
    <|> do
      ident <- identifier
      return $ VariableExpression ident
    <|> do
      exprs <- brackets expressions
      return $ ListExpression exprs
    <|> do
      membs <- braces members
      return $ ObjectExpression membs

members = sepBy member $ do
  whiteSpace
  char ','
  whiteSpace

member = do
  ident <- identifier <|> stringLiteral
  whiteSpace
  string ":"
  whiteSpace
  expr <- expression
  return (ident, expr)


-- cons, lambda, object
