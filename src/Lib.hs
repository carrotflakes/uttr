{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import System.IO
import Data.Map
import Control.Monad
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Parser
import Engine
import Print


someFunc :: IO ()
someFunc' = test >>= repl
someFunc = do
  input <- readFile "./examples/foo.uttr"
  case parseStatements "file" input of
    Right sts -> foldM ep initialEnv sts >> return ()

    Left err -> do
      putStrLn $ "Parsing error: " ++ show err


repl env = do
  putStr "> " >> hFlush stdout
  line <- getLine
  env <- rep env line
  repl env


rep env str =
  case parseStatement "input" str of
    Right st -> ep env st

    Left err -> do
      putStrLn $ "Parsing error: " ++ str
      return env

ep env st = do
  res <- doStatement env st
  case res of
    Right (env', result) -> do
      T.putStrLn $ showU result
      return env'

    Left err -> do
      T.putStrLn $ maybe "Backtracked" id err
      return env


test = foldM f initialEnv
  [ "1"
  , "true"
  , "null"
  , "\"hoge\""
  , "1 + 2 * 3"
  , "( 1 + 2 ) * 3"
  , "a = 1"
  , "b = [1, \"a\"]"
  , "c = {a: 1, \"b\": \"2\"}"

  , "[ a = 1 ] ( 2 )"
  , "[ 1 = 2, 2 = 3 ] ( 2 )"
  , "[ 1 = 2, x | false = 3, x | true = 4 ] ( 2 )"

  , "f0 ( x ) = x + 1"
  , "f0 ( 2 )"
  , "range x|x>0=(x-1):range(x-1)|x==0=[]"
  , "range(10)"
    ]
  where
    f env src = do
      putStrLn $ "> " ++ src
      rep env src
