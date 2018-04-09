module Main where

import Parser
import Command
import Bank
import System.IO
import Database

main :: IO ()
main = do
  putStrLn "Welcome to bank-app"
  putStrLn "-------------------"
  putStrLn "List of commands: "
  putStrLn $ unlines commands
  a <- loadBank
  b <- loop a
  saveBank b
  return ()

prompt :: IO ()
prompt = do
  putStr "bank > "
  hFlush stdout


loop :: Bank -> IO Bank
loop b = do
  prompt
  l <- getLine
  c <- doRun l
  (res, bank') <- doEvalCommand c b
  let bank''= performPending bank'
  printResult (res, bank'')
  if shouldExit c
    then return bank''
    else loop bank''

shouldExit :: Maybe Command -> Bool
shouldExit (Just QuitCmd) = True
shouldExit _ = False

printResult ::  (String, Bank) -> IO()
printResult (s, b) = do
  putStrLn "Result: "
  putStrLn s
  return ()
