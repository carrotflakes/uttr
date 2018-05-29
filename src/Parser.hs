{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

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
    P.reservedNames = ["import", "true", "false", "null"]
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
  st <- (DefinitionStatement <$> try definition <|>
         ExpressionStatement <$> try (expression ExpressionMode) <|>
         importStatement)
  Text.Parsec.optional semi
  return st

importStatement = do
  whiteSpace
  string "import"
  whiteSpace
  fi <- ((FileIndicator . T.unpack) <$> stringLiteral <|>
         (ShortFileIndicator . T.unpack) <$> identifier)
  return $ ImportStatement fi

definition = try functionDefinition <|> constantDefinition

functionDefinition = do
  whiteSpace
  ident <- identifier
  whiteSpace
  block <- block
  return $ FunctionDefinition ident block

constantDefinition = do
  whiteSpace
  ident <- identifier
  whiteSpace
  string "="
  whiteSpace
  expr <- expression ExpressionMode
  return $ ConstantDefinition ident expr


patterns = parens (expressions PatternMode) <|> do
  pattern <- expression PatternMode
  return [pattern]

expression mode = operatorSystem mode
expressions mode = sepBy (expression mode) comma


operatorSystem mode = buildExpressionParser (operators mode) (applies mode)

operators mode =
  [ [ Prefix (reservedOp "-" >> return (\x -> apply "-" [x]))
    ]
  , [ Infix (reservedOp "@" >> return AtPattern) AssocNone
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
  , [ infixOp "&&" AssocLeft
    , infixOp "||" AssocLeft
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
      f (ApplyExpression (ValueExpression $ StringValue $ T.pack "[]")
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
  <|> templateLiteral mode
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
    return $ PropertyMember ident expr)
  <|> (do
    ident <- identifier
    return $ PropertyMember ident (VariableExpression ident))
  <|> (do
    string "..."
    whiteSpace
    expr <- expression mode
    return $ SpreadMember expr)

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


templateLiteral mode = do
  whiteSpace
  char '`'
  xs <- many $ (Left <$> try expressionPart) <|> (Right . T.pack <$> stringPart)
  char '`'
  whiteSpace
  return $ TemplateLiteralExpression xs
  where
    expressionPart  = between (char '$') (char '$') (expression mode)

    stringPart      = foldr (maybe id (:)) "" <$> many1 tlChar

    tlChar          = try dollar
                    <|> (Just <$> endOfLine)
                    <|> (notFollowedBy (oneOf "$`") >> stringChar)

    dollar          = string "$$" >> return (Just '$')


    stringChar      =   do{ c <- stringLetter; return (Just c) }
                    <|> stringEscape
                    <?> "string character"

    stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape    = do{ char '\\'
                        ;     do{ escapeGap  ; return Nothing }
                          <|> do{ escapeEmpty; return Nothing }
                          <|> do{ esc <- escapeCode; return (Just esc) }
                        }

    escapeEmpty     = char '&'
    escapeGap       = do{ many1 space
                        ; char '\\' <?> "end of string gap"
                        }



    -- escape codes
    escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                    <?> "escape code"

    charControl     = do{ char '^'
                        ; code <- upper
                        ; return (toEnum (fromEnum code - fromEnum 'A' + 1))
                        }

    charNum         = do{ code <- decimal
                                    <|> (P.octal lexer)
                                    <|> (P.hexadecimal lexer)
    --                              <|> do{ char 'o'; number 8 octDigit }
    --                              <|> do{ char 'x'; number 16 hexDigit }
                        ; if code > 0x10FFFF
                          then fail "invalid escape sequence"
                          else return (toEnum (fromInteger code))
                        }

    charEsc         = choice (map parseEsc escMap)
                    where
                      parseEsc (c,code)     = do{ char c; return code }

    charAscii       = choice (map parseAscii asciiMap)
                    where
                      parseAscii (asc,code) = try (do{ string asc; return code })


    -- escape code tables
    escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
    asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

    ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                       "FS","GS","RS","US","SP"]
    ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                       "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                       "CAN","SUB","ESC","DEL"]

    ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                       '\EM','\FS','\GS','\RS','\US','\SP']
    ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                       '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                       '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


    -- auxility
    decimal = P.decimal lexer
