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


initialEnv = fromList $ fmap (\x -> (x, StringValue x)) ["*", "/", "%", "+", "-"]


evalStatement :: Env -> Statement -> Either (Maybe String) (Env, String)
evalStatement env (ExpressionStatement expr) =
  case evalExpression ([], env) expr of
    Right evaled -> Right (env, show evaled)
    Left err -> Left err

evalStatement env (DefinitionStatement (FunctionDefinition ident params body))
  = case Map.lookup ident env of
  Just (FunctionValue ident clauses)
    -> Right (Map.insert ident func env, show func)
    where func = FunctionValue ident $ clauses ++ [(params, body)]
  Nothing
    -> Right (Map.insert ident func env, show func)
    where func = FunctionValue ident [(params, body)]
  Just value -> Left $ Just $ "Cannot overwrite with function: " ++ show value

-- TODO matchで書く
evalStatement env (DefinitionStatement (ConstantDefinition ident expr))
  = case evalExpression ([], env) expr of
  Right evaled -> Right (Map.insert ident evaled env, show evaled)
  Left err -> Left err


evalExpression :: Scope -> Expression -> Either (Maybe String) Value
evalExpression scope (ValueExpression value) = Right value

evalExpression scope (VariableExpression ident)
  = case resolve scope ident of
  Just value -> Right value
  Nothing -> Left $ Just $ "Cannot resolved: " ++ ident

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


apply :: Scope -> Value -> [Value] -> Either (Maybe String) Value
apply scope (FunctionValue ident clauses) args = f clauses
  where
    f [] = Left Nothing
    f (clause:clauses) = case applyFunction scope clause args of
      right@(Right _) -> right
      Left Nothing -> f clauses
      left@(Left (Just err)) -> left

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
    _ -> Left $ Just $ "Unexpected argument of + operator: " ++ show params

apply scope (StringValue "-") params =
  case params of
    [NumberValue n1, NumberValue n2] -> Right $ NumberValue (n1 - n2)
    _ -> Left $ Just $ "Unexpected argument of - operator: " ++ show params

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
  | idx < 0 || length values <= idx = Left $ Nothing -- ここの挙動悩む
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

apply scope value _ = Left $ Just $ "Not function: " ++ show value


applyFunction :: Scope -> ([Expression], Expression) -> [Value] -> Either (Maybe String) Value
applyFunction (alist, env) (params, body) args
  = case match (ListExpression params) (ListValue args) of
  Just bindings -> evalExpression (bindings ++ alist, env) body
  Nothing -> Left Nothing


match :: Expression -> Value -> Maybe [(Identifier, Value)]
match = match' []
match' alist (ValueExpression value) value'
  | value == value' = Just alist
  | otherwise = Nothing

match' alist (VariableExpression ident) value
  = case Data.List.lookup ident alist of
  Just value' | value == value' -> Just alist
  Just _ -> Nothing
  Nothing -> Just $ (ident, value) : alist

match' alist (ListExpression exprs) (ListValue values)
  | length exprs == length values = foldM f alist $ zip exprs values
  | otherwise = Nothing
  where f alist (e, v) = match' alist e v

match' alist (ApplyExpression _ _) _ = error "kokoni kurunoha okasii"

match' alist expr value = error $ "Cannot match " ++ show expr ++ " with " ++ show value


resolve :: Scope -> Identifier -> Maybe Value
resolve (alist, env) ident = msum [Data.List.lookup ident alist, Map.lookup ident env]


truthy :: Value -> Bool
truthy (BoolValue False) = False
truthy NilValue = False
truthy _ = True
