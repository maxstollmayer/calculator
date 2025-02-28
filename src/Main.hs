module Main where

import Lib
import System.Environment (getArgs)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

main :: IO ()
main = do
  args <- getArgs
  if "--help" `elem` args || "-h" `elem` args
    then printCLIHelp
    else
      if null args
        then runREPL
        else runCLI args

runREPL :: IO ()
runREPL = do
  putStrLn "Calculator:"
  putStrLn "Type an expression to calculate, 'help' or 'quit'."
  loopREPL

loopREPL :: IO ()
loopREPL = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case input of
    "help" -> printREPLHelp >> loopREPL
    "quit" -> return ()
    _ -> case calculate input of
      Left err -> putStrLn err >> loopREPL
      Right res -> print res >> loopREPL

runCLI :: [String] -> IO ()
runCLI inputs =
  if length inputs == 1
    then f (head inputs)
    else mapM_ f inputs
  where
    f :: String -> IO ()
    f input = case calculate input of
      Left err -> hPutStrLn stderr err
      Right res -> print res

printCLIHelp :: IO ()
printCLIHelp = do
  putStrLn "Usage: calculate [OPTION] [EXPRESSION...]"
  putStrLn ""
  putStrLn "Evaluate basic arithmetic expressions."
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  -h, --help    Show this help message and exit"
  putStrLn ""
  putStrLn "Expressions:"
  putStrLn "Consist of the following building blocks."
  putStrLn "  Number  .......... integers and floats"
  putStrLn "  Parentheses ...... (x)"
  putStrLn "  Addition ......... x + y"
  putStrLn "  Subtraction ...... x - y"
  putStrLn "  Multiplication ... x * y"
  putStrLn "  Division ......... x / y"
  putStrLn ""
  putStrLn "REPL Mode:"
  putStrLn "  If no argument is provided, it will enter REPL mode and evaluate expressions interactively."

printREPLHelp :: IO ()
printREPLHelp = do
  putStrLn "Evaluate basic arithmetic expression."
  putStrLn ""
  putStrLn "Expressions consist of the following building blocks."
  putStrLn "  Number  .......... integers and floats"
  putStrLn "  Parentheses ...... (x)"
  putStrLn "  Addition ......... x + y"
  putStrLn "  Subtraction ...... x - y"
  putStrLn "  Multiplication ... x * y"
  putStrLn "  Division ......... x / y"
  putStrLn ""
  putStrLn "Type 'quit' to quit the REPL."
  putStrLn "Type 'help' to show this message again."
