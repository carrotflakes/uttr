{-# LANGUAGE OverloadedStrings #-}

module Print
       where


import Data.List
import qualified Data.Text as T
import Text.Show.Unicode

import Ast


class ShowU a where
  showU :: a -> Text


join = foldr1 T.append


instance ShowU Statement where
  showU (DefinitionStatement definition) = showU definition
  showU (ExpressionStatement expression) = showU expression

instance ShowU Definition where
  showU (FunctionDefinition ident block)
    = join $ [ident, " "] ++ intersperse ", " (map showMC block)
    where
      showMC (patterns, Left body, whereClause) = join [showUPatterns patterns, " = ", showU body, showWC whereClause]
      showMC (patterns, Right guardClauses, whereClause) = join $ [showUPatterns patterns] ++ (map showGC guardClauses) ++ [showWC whereClause]
      showGC (guard, body) = join [" | ", showU guard, " = ", showU body]
      showWC [] = ""
      showWC defs = join $ [" {"] ++ intersperse "; " (map showU defs) ++ ["}"]
  showU (ConstantDefinition ident body)
    = join [ident, " = ", showU body]

instance ShowU Expression where
  showU (ValueExpression value) = showU value
  showU (VariableExpression ident) = ident
  showU (ApplyExpression func args)
    = join $ [showU func, "("] ++ intersperse ", " (map showU args) ++ [")"]
  showU (ListExpression elms) = join $ ["["] ++ intersperse ", " (map showU elms) ++ ["]"]
  showU (ObjectExpression membs)
    = join $ ["{"] ++ intersperse ", " (map showMember membs) ++ ["}"]
    where
      showMember (PropertyMember key expr) = join [T.pack $ ushow key, ": ", showU expr]
      showMember (SpreadMember expr) = join ["...", showU expr]
  showU (ConsExpression car cdr) = join [showU car, ":", showU cdr]
  showU (ClosureExpression block) = join $ ["["] ++ intersperse ", " (map showMC block) ++ ["]"]
    where
      showMC (patterns, Left body, whereClause) = join [showUPatterns patterns, " = ", showU body, showWC whereClause]
      showMC (patterns, Right guardClauses, whereClause) = join $ [showUPatterns patterns] ++ map showGC guardClauses ++ [showWC whereClause]
      showGC (guard, body) = join [" | ", showU guard, " = ", showU body]
      showWC [] = ""
      showWC defs = join $ [" {"] ++ intersperse "; " (map showU defs) ++ ["}"]

instance ShowU Value where
  showU (NumberValue value)
    | fromIntegral (round value) == value = T.pack $ show $ round value
    | otherwise = T.pack $ show value
  showU (StringValue value) = T.pack $ ushow value
  showU (BoolValue True) = "true"
  showU (BoolValue False) = "false"
  showU NullValue = "null"
  showU (ListValue elms) = join $ ["["] ++ intersperse ", " (map showU elms) ++ ["]"]
  showU (ObjectValue membs) = join $ ["{"] ++ intersperse ", " (map showMember membs) ++ ["}"]
    where showMember (key, value) = join [key, ": ", showU value]
  showU (FunctionValue ident _) = join ["<function ", ident, ">"]
  showU (ClosureValue _ matchClauses) = "<closure>"
  showU (VariableValue ident) = ident

showUPatterns [pattern] = showU pattern
showUPatterns patterns = join $ ["("] ++ intersperse ", " (map showU patterns) ++ [")"]


instance (ShowU a) => ShowU [a] where
  showU list = join $ ["["] ++ intersperse ", " (map showU list) ++ ["]"]
