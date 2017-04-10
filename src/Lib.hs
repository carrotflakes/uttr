module Lib
    ( someFunc
    ) where

import System.IO
import Data.Map
import Control.Monad
import Control.Applicative

import Parser (parseStatement)
import Engine


someFunc :: IO ()
someFunc = test >>= repl


repl env = do
  putStr "> " >> hFlush stdout
  line <- getLine
  env <- rep env line
  repl env


rep env str =
  case parseStatement "input" str of
    Right st -> case evalStatement env st of

      Right (env', result) -> do
        putStrLn $ result
        return env'

      Left err -> do
        putStrLn $ maybe "Backtracked" id err
        return env

    Left err -> do
      putStrLn $ "Oops! " ++ show err
      return env


test = foldM f initialEnv [
    "1",
    "t",
    "nil",
    "\"hoge\"",
    "1+2",
    "1 + 2",
    "1 + 2 * 3",
    "( 1 + 2 ) * 3",
    "a = 1",
    "b = [1, \"a\"]",
    "c = {a: 1, \"b\": \"2\"}"
    ]
  where
    f env src = do
      putStrLn $ "> " ++ src
      rep env src
