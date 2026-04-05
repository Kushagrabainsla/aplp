{-
  Name: Kushagra Bainsla
  Class: CS 252
  Assigment: HW3
  Date: March 24, 2026
  Description: Driver program for running .imp files
-}

module Main where

import System.Environment
import qualified Data.Map as Map
import Parser
import WhileInterp

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: runhaskell Main.hs <filename.imp>"
    else do
      let filename = head args
      result <- parseFile filename
      case result of
        Left parseErr -> do
          putStrLn "Parse error:"
          print parseErr
        Right expr -> do
          putStrLn $ "Parsed expression: " ++ show expr
          putStrLn ""
          case run expr of
            Left runtimeErr -> putStrLn runtimeErr
            Right (value, store) -> do
              putStrLn $ "Final value: " ++ show value
              putStrLn "Final store:"
              mapM_ printBinding (Map.toList store)

printBinding :: (String, Value) -> IO ()
printBinding (var, val) = putStrLn $ "  " ++ var ++ " = " ++ showValue val

showValue :: Value -> String
showValue (IntVal n) = show n
showValue (BoolVal b) = if b then "true" else "false"
