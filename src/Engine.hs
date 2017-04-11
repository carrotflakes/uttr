module Engine
    ( evalStatement,
      initialEnv,
      Env
    ) where

import Data.Map as Map
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad
import Control.Applicative


import Ast


initialEnv = fromList $ fmap (\x -> (x, StringValue x)) ["*", "/", "%", "+", "-", "str"]


evalStatement :: Env -> Statement -> Either (Maybe String) (Env, String)
evalStatement env (ExpressionStatement expr) =
  case evalExpression ([], env) expr of
    Right evaled -> Right (env, show evaled)
    Left err -> Left err

evalStatement env (DefinitionStatement (FunctionDefinition ident matchClauses))
  = case Map.lookup ident env of
  Just (FunctionValue ident matchClauses')
    -> Right (Map.insert ident func env, show func)
    where func = FunctionValue ident $ matchClauses' ++ matchClauses
  Nothing
    -> Right (Map.insert ident func env, show func)
    where func = FunctionValue ident matchClauses
  Just value -> Left $ Just $ "Cannot overwrite with function: " ++ show value

-- TODO matchで書く
evalStatement env (DefinitionStatement (ConstantDefinition ident expr))
  = case evalExpression ([], env) expr of
  Right evaled -> Right (Map.insert ident evaled env, show evaled)
  Left err -> Left err


evalExpression :: Scope -> Expression -> Either (Maybe String) Value
evalExpression scope (VariableExpression ident)
  = case resolve scope ident of
  Just value -> Right value
  Nothing -> Left $ Just $ "Cannot resolved: " ++ ident

evalExpression scope (ValueExpression value) = Right value

evalExpression scope (ListExpression exprs)
  = case find isLeft evaleds of
  Just left -> left
  Nothing -> Right $ ListValue $ rights evaleds
  where evaleds = fmap (evalExpression scope) exprs

evalExpression scope (ObjectExpression membs)
  = case find isLeft evaleds of
  Just left -> left
  Nothing -> Right $ ObjectValue $ zip (fmap fst membs) (rights evaleds)
  where evaleds = fmap (evalExpression scope) $ fmap snd membs

evalExpression scope (ApplyExpression opExpr paramExprs) =
  case find isLeft evaledExprs of
    Just left -> left
    Nothing -> apply scope op params
  where
    evaledExprs = fmap (evalExpression scope) (opExpr : paramExprs)
    op : params = rights evaledExprs

evalExpression scope (ConsExpression carExpr cdrExpr)
  = case (evalExpression scope carExpr, evalExpression scope cdrExpr) of
  (Right carValue, Right (ListValue elms)) -> Right $ ListValue $ carValue : elms
  (Left err, _) -> Left err
  (_, Right value) -> Left $ Just $ "The expression must be evaluated as a list: " ++ show cdrExpr
  (_, Left err) -> Left err

evalExpression scope (ClosureExpression matchClauses) = Right $ ClosureValue scope matchClauses


apply :: Scope -> Value -> [Value] -> Either (Maybe String) Value
apply (alist, env) (FunctionValue ident matchClauses) args
  = applyFunction ([], env) matchClauses args

apply _ (ClosureValue scope matchClauses) args = applyFunction scope matchClauses args

apply scope (StringValue "*") params =
  case params of
    [NumberValue n1, NumberValue n2] -> Right $ NumberValue (n1 * n2)
    _ -> Left $ Just $ "Unexpected argument of * operator: " ++ show params

apply scope (StringValue "/") params =
  case params of
    [NumberValue n1, NumberValue n2] -> Right $ NumberValue (n1 / n2)
    _ -> Left $ Just $ "Unexpected argument of / operator: " ++ show params

apply scope (StringValue "+") params =
  case params of
    [NumberValue n1, NumberValue n2] -> Right $ NumberValue (n1 + n2)
    [StringValue s1, StringValue s2] -> Right $ StringValue (s1 ++ s2)
    _ -> Left $ Just $ "Unexpected argument of + operator: " ++ show params

apply scope (StringValue "-") params =
  case params of
    [NumberValue n1, NumberValue n2] -> Right $ NumberValue (n1 - n2)
    _ -> Left $ Just $ "Unexpected argument of - operator: " ++ show params

apply scope (StringValue "%") params =
  case params of
    [NumberValue n1, NumberValue n2] -> Right $ NumberValue $ fromIntegral (round n1 `mod` round n2) -- FIXME
    _ -> Left $ Just $ "Unexpected argument of % operator: " ++ show params

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
  | truthy rhs = Right rhs
  | otherwise = Right $ BoolValue False

apply scope (StringValue "[]") [ListValue values, NumberValue idxNum]
  | idx < 0 || length values <= idx = Left $ Nothing
  | fromIntegral idx /= idxNum = Left $ Nothing
  | otherwise = Right $ values !! idx
  where idx = round idxNum
apply scope (StringValue "[]") [ListValue values, _]
  = Left $ Nothing
apply scope (StringValue "[]") [ObjectValue members, NumberValue idx]
  = case Data.List.lookup (show idx) members of
  Just value -> Right $ value
  Nothing -> Left $ Nothing
apply scope (StringValue "[]") [ObjectValue members, StringValue key]
  = case Data.List.lookup key members of
  Just value -> Right $ value
  Nothing -> Left $ Nothing
apply scope (StringValue "[]") [ObjectValue members, _]
  = Left $ Nothing
apply scope (StringValue "[]") [value, _]
  = Left $ Just $ "Cannot get member from: " ++ show value

apply scope (StringValue "str") [param] = Right $ StringValue $ show param

apply scope value _ = Left $ Just $ "Not function: " ++ show value


applyFunction :: Scope -> [([Expression], Either Expression [(Expression, Expression)])] -> [Value] -> Either (Maybe String) Value
applyFunction scope@(alist, env) matchClauses args = f matchClauses
  where
    f [] = Left Nothing
    f (matchClause:matchClauses) = case g matchClause of
      right@(Right _) -> right
      Left Nothing -> f matchClauses
      left@(Left (Just err)) -> left
    g (patterns, Left body) = case match scope (ListExpression patterns) (ListValue args) of
      Just bindings -> i bindings body
      Nothing -> Left Nothing
    g (patterns, Right guardClauses) = case match scope (ListExpression patterns) (ListValue args) of
      Just bindings -> h bindings guardClauses
      Nothing -> Left Nothing
    h bindings [] = Left Nothing
    h bindings ((guard, body):guardClauses) = do
      value <- i bindings guard
      if truthy value
        then i bindings body
        else h bindings guardClauses
    i bindings body = evalExpression (bindings ++ alist, env) body


match :: Scope -> Expression -> Value -> Maybe [(Identifier, Value)]
match scope = match' scope []

match' scope alist (ValueExpression (VariableValue ident)) value
  = match' scope alist (VariableExpression ident) value

match' scope alist (ValueExpression value) value'
  | value == value' = Just alist
  | otherwise = Nothing

match' scope alist (VariableExpression ident) value
  = case Data.List.lookup ident alist of
  Just value' | value == value' -> Just alist
  Just _ -> Nothing
  Nothing -> Just $ (ident, value) : alist

match' scope alist (ListExpression exprs) (ListValue values)
  | length exprs == length values = foldM f alist $ zip exprs values
  | otherwise = Nothing
  where f alist (e, v) = match' scope alist e v

match' scope alist (ObjectExpression membExprs) (ObjectValue membVals)
  = match' scope alist (ListExpression $ fmap fst pairs) (ListValue $ fmap snd pairs)
  where
    pairs = Data.Maybe.mapMaybe (\(k, v) -> ((,) v) `fmap` Data.List.lookup k membVals) membExprs

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


expressify (ListValue elms) = ListExpression $ fmap expressify elms
expressify (ObjectValue membs) = ObjectExpression $ fmap (\(k, v) -> (k, expressify v)) membs
expressify (VariableValue ident) = VariableExpression ident
expressify value = ValueExpression value
{-
  = NumberValue Double
  | StringValue String
  | BoolValue Bool
  | NullValue
  | FunctionValue Identifier [([Expression], Either Expression [(Expression, Expression)])]
  | ClosureValue Scope [([Expression], Either Expression [(Expression, Expression)])]
-}

resolve :: Scope -> Identifier -> Maybe Value
resolve (alist, env) ident = msum [Data.List.lookup ident alist, Map.lookup ident env]


truthy :: Value -> Bool
truthy (BoolValue False) = False
truthy NullValue = False
truthy _ = True
