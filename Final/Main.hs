{-
  Module: Main
  Description: Entry point for the language interpreter.
  Design Decisions:
    - Provides a command-line interface to run files.
    - Includes detailed error reporting for both parsing and runtime errors.
    - Demonstrates the final state of the environment for debugging/inspection.
-}

module Main where

import System.Environment
import System.IO
import Parser
import Interpreter
import AST
import qualified Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runFile filename
    _ -> putStrLn "Usage: runghc Main.hs <filename.lp>"

runFile :: String -> IO ()
runFile filename = do
  result <- parseFile filename
  case result of
    Left err -> do
      putStrLn "--- PARSE ERROR ---"
      print err
    Right program -> do
      case staticCheck program of
        Left staticErr -> do
          putStrLn "--- STATIC ANALYSIS ERROR ---"
          putStrLn staticErr
        Right _ -> do
          case runProgram program of
            Left runtimeErr -> do
              putStrLn "--- RUNTIME ERROR ---"
              putStrLn runtimeErr
            Right (val, env) -> do
              putStrLn "--- EXECUTION SUCCESSFUL ---"
              putStrLn $ "Final Result: " ++ showValue val
              putStrLn ""
              putStrLn "--- FINAL ENVIRONMENT ---"
              printEnv env

showValue :: Value -> String
showValue (IntVal n)  = show n
showValue (BoolVal b) = if b then "true" else "false"
showValue (ListVal vs) = "[" ++ intercalate ", " (map showValue vs) ++ "]"
  where
    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs
showValue (FunVal params _) = "<function(" ++ show params ++ ")>"
showValue NilVal = "nil"

printEnv :: Map.Map String Value -> IO ()
printEnv env = mapM_ printBinding (Map.toList env)
  where
    printBinding (k, v) = putStrLn $ "  " ++ k ++ " = " ++ showValue v
