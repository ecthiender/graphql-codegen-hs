module Main where

import Lib
import System.Environment (getArgs)
import qualified System.Exit as Sys

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "ERROR: no file provided"
      usage
      Sys.exitFailure
    (arg:_) -> do
      case arg of
        "--help" -> usage
        filepath -> do
          schema <- parseFile filepath
          let code = generateCode schema
          putStrLn code

usage :: IO ()
usage = do
  putStrLn "Usage: graphql-codegen FILE"
  putStrLn "Generate TypeScript types from GraphQL SDL files."
