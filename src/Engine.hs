{-# LANGUAGE OverloadedStrings #-}

module Engine
    ( doStatement,
      initialEnv,
      Env,
      CompileContext(..)
    ) where

import System.FilePath.Posix
import Data.Map as Map
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Show.Unicode


import Ast
import Print
import Parser


data CompileContext = CompileContext { executablePath :: String,
                                       currentDirectoryPath :: String,
                                       currentFilePath :: Maybe String
                                     }


show' :: ShowU a => a -> Text
show' = showU


initialEnv = fromList $ fmap (\x -> (x, StringValue x)) ["*", "/", "%", "+", "-", "str", "json", "findall"]


doStatement :: CompileContext -> Env -> Statement -> IO (Either (Maybe Text) (Env, Value))
doStatement cctx env (ExpressionStatement expr) = return $ case evalExpression ([], env) expr of
  Right evaled -> Right (env, evaled)
  Left err -> Left err

doStatement cctx env (DefinitionStatement def) = return $ do
  (ident, value) <- evalDefinition ([], env) def
  return (Map.insert ident value env, value)

doStatement cctx env (ImportStatement fi) = do
  input <- readFile path
  case parseStatements (show fi) input of
    Right sts -> f env sts
    Left err -> return $ Left $ Just $ "Parsing error: " `T.append` (T.pack $ show err)
  where
    path = filePath cctx fi
    f env [] = return $ Right (env, BoolValue True)
    f env (st:sts) = do
      let cctx' = cctx { currentFilePath = Just path }
      res <- doStatement cctx' env st
      case res of
        Right (env', result) -> f env' sts
        Left err -> return $ Left $ Just $ (maybe "Backtracked" id err) `T.append` " in " `T.append` (T.pack $ show fi)


evalDefinition :: Scope -> Definition -> Either (Maybe Text) (Identifier, Value)
evalDefinition scope (FunctionDefinition ident matchClauses) = case resolve scope ident of
  Just (FunctionValue ident matchClauses') -> Right (ident, FunctionValue ident $ matchClauses' ++ matchClauses)
  Nothing -> Right (ident, FunctionValue ident matchClauses)
  Just value -> Left $ Just $ "Cannot overwrite with function: " `T.append` show' value

evalDefinition scope (ConstantDefinition ident expr) = do
  evaled <- evalExpression scope expr
  return (ident, evaled)


evalExpression :: Scope -> Expression -> Either (Maybe Text) Value
evalExpression scope (VariableExpression ident)
  = case resolve scope ident of
  Just value -> Right value
  Nothing -> Left $ Just $ "Cannot resolved: " `T.append` ident

evalExpression scope (ValueExpression value) = Right value

evalExpression scope (ListExpression exprs)
  = case find isLeft evaleds of
  Just left -> left
  Nothing -> Right $ ListValue $ rights evaleds
  where evaleds = fmap (evalExpression scope) exprs

evalExpression scope (ObjectExpression membs)
  = do
    evaledMembers <- mapM evalMember membs
    return $ ObjectValue $ unique $ concat evaledMembers
  where
    evalMember (PropertyMember key expr) = do
      value <- evalExpression scope expr
      return [(key, value)]
    evalMember (SpreadMember expr) = do
      value <- evalExpression scope expr
      case value of
        ObjectValue membs -> return membs
        _ -> Left $ Just $ "The expression must be evaluated as a object: " `T.append` show' expr

    unique = reverse . nubBy (\x y -> fst x == fst y) . reverse

evalExpression scope (ApplyExpression opExpr paramExprs) = do
  op <- evalExpression scope opExpr
  let evaledExprs = fmap (evalExpression scope) paramExprs
  case find isLeft evaledExprs of
    Just left -> left
    Nothing -> apply scope op $ rights evaledExprs

evalExpression scope (ConsExpression carExpr cdrExpr)
  = case (evalExpression scope carExpr, evalExpression scope cdrExpr) of
  (Right carValue, Right (ListValue elms)) -> Right $ ListValue $ carValue : elms
  (Left err, _) -> Left err
  (_, Right value) -> Left $ Just $ "The expression must be evaluated as a list: " `T.append` show' cdrExpr
  (_, Left err) -> Left err

evalExpression scope (ClosureExpression block) = Right $ ClosureValue scope block

evalExpression scope (TemplateLiteralExpression xs)
  = case find isLeft evaleds of
  Just (Left err) -> Left err
  Nothing -> Right $ StringValue $ T.concat $ rights evaleds
  where
    evaleds = fmap f xs
    f (Left expr) = case evalExpression scope expr of
      Right (StringValue value) -> Right value
      Right _ -> Left $ Just $ "The expression in template literal must be evaluated as a string: " `T.append` show' expr
      Left err -> Left err
    f (Right text) = Right text


apply :: Scope -> Value -> [Value] -> Either (Maybe Text) Value
apply (alist, env) (FunctionValue ident block) args
  = applyFunction ([], env) block args

apply _ (ClosureValue scope block) args = applyFunction scope block args

apply scope (StringValue "*") params =
  case params of
    [NumberValue n1, NumberValue n2] -> Right $ NumberValue (n1 * n2)
    _ -> Left $ Just $ "Unexpected argument of * operator: " `T.append` show' params

apply scope (StringValue "/") params =
  case params of
    [NumberValue n1, NumberValue n2] -> Right $ NumberValue (n1 / n2)
    _ -> Left $ Just $ "Unexpected argument of / operator: " `T.append` show' params

apply scope (StringValue "+") params =
  case params of
    [NumberValue n1, NumberValue n2] -> Right $ NumberValue (n1 + n2)
    [StringValue s1, StringValue s2] -> Right $ StringValue (s1 `T.append` s2)
    [ListValue l1, ListValue l2] -> Right $ ListValue (l1 ++ l2)
    _ -> Left $ Just $ "Unexpected argument of + operator: " `T.append` show' params

apply scope (StringValue "-") params =
  case params of
    [NumberValue n1] -> Right $ NumberValue (negate n1)
    [NumberValue n1, NumberValue n2] -> Right $ NumberValue (n1 - n2)
    _ -> Left $ Just $ "Unexpected argument of - operator: " `T.append` show' params

apply scope (StringValue "%") params =
  case params of
    [NumberValue n1, NumberValue n2] -> Right $ NumberValue $ fromIntegral (round n1 `mod` round n2) -- FIXME
    _ -> Left $ Just $ "Unexpected argument of % operator: " `T.append` show' params

apply scope (StringValue "==") [lhs, rhs]
  = Right $ BoolValue $ lhs == rhs
apply scope (StringValue "!=") [lhs, rhs]
  = Right $ BoolValue $ lhs /= rhs

apply scope (StringValue ">") [NumberValue n1, NumberValue n2]
  = Right $ BoolValue $ n1 > n2
apply scope (StringValue "<") [NumberValue n1, NumberValue n2]
  = Right $ BoolValue $ n1 < n2
apply scope (StringValue ">=") [NumberValue n1, NumberValue n2]
  = Right $ BoolValue $ n1 >= n2
apply scope (StringValue "<=") [NumberValue n1, NumberValue n2]
  = Right $ BoolValue $ n1 <= n2

apply scope (StringValue "&&") [lhs, rhs]
  | truthy lhs && truthy rhs = Right rhs
  | otherwise = Right $ BoolValue False
apply scope (StringValue "||") [lhs, rhs]
  | truthy lhs = Right lhs
  | otherwise = Right rhs

apply scope (StringValue "[]") [ListValue values, NumberValue idxNum]
  | idx < 0 || length values <= idx = Left $ Nothing
  | fromIntegral idx /= idxNum = Left $ Nothing
  | otherwise = Right $ values !! idx
  where idx = round idxNum
apply scope (StringValue "[]") [ListValue values, _]
  = Left $ Nothing
apply scope (StringValue "[]") [ObjectValue members, num@(NumberValue idx)]
  = case Data.List.lookup (show' num) members of
  Just value -> Right $ value
  Nothing -> Left $ Nothing
apply scope (StringValue "[]") [ObjectValue members, StringValue key]
  = case Data.List.lookup key members of
  Just value -> Right $ value
  Nothing -> Left $ Nothing
apply scope (StringValue "[]") [ObjectValue members, _]
  = Left $ Nothing
apply scope (StringValue "[]") [value, _]
  = Left $ Just $ "Cannot get member from: " `T.append` show' value

apply scope (StringValue "str") [param] = Right $ StringValue $ case param of
  StringValue string -> string
  value -> show' value

apply scope (StringValue "json") [param] = case check param of
  Nothing -> Right $ StringValue $ show' param
  Just value -> Left $ Just $ "Cannot jsonify: " `T.append` show' value
  where
    check (ListValue elms) = msum $ fmap check elms
    check (ObjectValue membs) = msum $ fmap (check . snd) membs
    check v@(FunctionValue _ _) = Just v
    check v@(ClosureValue _ _) = Just v
    check v@(VariableValue _) = Just v
    check _ = Nothing

apply scope value _ = Left $ Just $ "Not function: " `T.append` show' value


applyFunction :: Scope -> Block -> [Value] -> Either (Maybe Text) Value
applyFunction scope matchClauses args = f matchClauses
  where
    f [] = Left Nothing
    f (matchClause:matchClauses) = case g matchClause of
      right@(Right _) -> right
      Left Nothing -> f matchClauses
      left@(Left (Just err)) -> left

    g :: ([Expression], Either Expression [(Expression, Expression)], [Definition]) -> Either (Maybe Text) Value
    g (patterns, bodyOrGuardClauses, defs)
      = case match scope (ListExpression patterns) (ListValue args) of
      Just bindings -> do
        scope' <- procWhereClause (Data.List.foldr bind scope bindings) defs
        case bodyOrGuardClauses of
          Left body -> evalExpression scope' body
          Right guardClauses -> h scope' guardClauses
      Nothing -> Left Nothing

    h scope [] = Left Nothing
    h scope ((guard, body):guardClauses) = do
      value <- evalExpression scope guard
      if truthy value
        then evalExpression scope body
        else h scope guardClauses

    procWhereClause :: Scope -> [Definition] -> Either (Maybe Text) Scope
    procWhereClause scope defs = foldM procDefs scope defs
      where
        procDefs scope def = do
          binding <- evalDefinition' scope def
          return $ bind binding scope

    evalDefinition' :: Scope -> Definition -> Either (Maybe Text) (Identifier, Value)
    evalDefinition' scope (FunctionDefinition ident matchClauses) = return binding
        where binding = (ident, ClosureValue (bind binding scope) matchClauses)

    evalDefinition' scope (ConstantDefinition ident expr) = do
      evaled <- evalExpression scope expr
      return (ident, evaled)


match :: Scope -> Expression -> Value -> Maybe [(Identifier, Value)]
match scope = match' scope []

match' scope alist (ValueExpression (VariableValue ident)) value
  = match' scope alist (VariableExpression ident) value

match' scope alist (ValueExpression value) value'
  | value == value' = Just alist
  | otherwise = Nothing

match' scope alist (VariableExpression ident) value
  | isWildcard ident = Just $ (ident, value) : alist
  | otherwise = case Data.List.lookup ident alist of
      Just value' | value == value' -> Just alist
      Just _ -> Nothing
      Nothing -> Just $ (ident, value) : alist

match' scope alist (ListExpression exprs) (ListValue values)
  | length exprs == length values = foldM f alist $ zip exprs values
  | otherwise = Nothing
  where f alist (e, v) = match' scope alist e v

match' scope alist objExpr@(ObjectExpression membs) (ObjectValue membVals)
  = match' scope alist (ListExpression $ fmap fst pairs) (ListValue $ fmap snd pairs)
  where
    pairs = Data.Maybe.mapMaybe f membs
    f (PropertyMember key expr) = ((,) expr) `fmap` Data.List.lookup key membVals
    f memb@(SpreadMember _) = error $ "Cannot use spread notation as pattern: " ++ show objExpr

match' scope alist (ConsExpression carExpr cdrExpr) (ListValue values)
  | length values /= 0 = do
    alist <- match' scope alist carExpr $ head values
    match' scope alist cdrExpr $ ListValue $ tail values
  | otherwise = Nothing

match' scope alist applyExpr@(ApplyExpression _ _) value
  = case evalExpression scope applyExpr of
  Right value' -> match' scope alist (expressify value') value
  Left _ -> Nothing -- TODO: Left (Just _) だったときの挙動

match' scope alist expr value = error $ "Cannot match " ++ show expr ++ " with " ++ show value


bind binding (alist, env) = (binding : alist, env)

scopeEnv (alist, env) = fromList alist `Map.union` env

expressify (ListValue elms) = ListExpression $ fmap expressify elms
expressify (ObjectValue membs) = ObjectExpression $ fmap (\(k, v) -> PropertyMember k (expressify v)) membs
expressify (VariableValue ident) = VariableExpression ident
expressify value = ValueExpression value -- NumberValue StringValue BoolValue NullValue FunctionValue ClosureValue


resolve :: Scope -> Identifier -> Maybe Value
resolve (alist, env) ident = msum [Data.List.lookup ident alist, Map.lookup ident env]


truthy :: Value -> Bool
truthy (BoolValue False) = False
truthy NullValue = False
truthy _ = True


isWildcard = T.isPrefixOf "_"


filePath cctx (FileIndicator path)
  = joinPath [currentDirectoryPath cctx,
              takeDirectory $ maybe "" id $ currentFilePath cctx,
              path]
filePath cctx (ShortFileIndicator name) = undefined
