module Main where

import Lib
import Parser
import Command
import Bank
import System.IO

main :: IO ()
main = do
  b <- loop testBank
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
  bank'' <- return (performPending bank')
  printResult (res, bank'')
  if shouldExit c
    then return bank''
    else loop bank''

shouldExit :: Maybe Command -> Bool
shouldExit (Just QuitCmd) = True
shouldExit _ = False

printResult ::  (String, Bank) -> IO()
printResult (s, b) = do
  putStr "Bank: "
  putStrLn $ show b
  putStr "Result: "
  putStrLn s
  return ()
