{-# LANGUAGE OverloadedStrings #-}

module Uttr
    ( standAlone,
      repl,
      rep,
      ep
    ) where

import System.IO
import System.Environment (getArgs, getExecutablePath)
import System.Directory
import Data.Map
import Control.Monad
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Parser
import Engine
import Print


standAlone :: IO ()
standAlone = do
  args <- getArgs
  executablePath <- getExecutablePath
  currentDirectoryPath <- getCurrentDirectory

  if length args >= 1
    then do
      let file = args !! 0
      input <- readFile file
      let cctx = CompileContext { executablePath = executablePath,
                                  currentDirectoryPath = currentDirectoryPath,
                                  currentFilePath = Just file
                                }
      case parseStatements "file" input of
        Right sts -> foldM (ep cctx) initialEnv sts >> return ()

        Left err -> do
          putStrLn $ "Parsing error: " ++ show err

    else do
      let cctx = CompileContext { executablePath = executablePath,
                                  currentDirectoryPath = currentDirectoryPath,
                                  currentFilePath = Nothing
                                }
      repl cctx initialEnv


repl cctx env = do
  putStr "> " >> hFlush stdout
  line <- getLine
  env <- rep cctx env line
  repl cctx env


rep cctx env str =
  case parseStatement "input" str of
    Right st -> ep cctx env st

    Left err -> do
      putStrLn $ "Parsing error: " ++ str
      return env

ep cctx env st = do
  res <- doStatement cctx env st
  case res of
    Right (env', result) -> do
      T.putStrLn $ showU result
      return env'

    Left err -> do
      T.putStrLn $ maybe "Backtracked" id err
      return env
