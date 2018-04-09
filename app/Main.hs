module Main where

import           Bank
import           Command
import           Control.Monad.Trans.Class
import           Database
import           Parser
import           System.Console.Haskeline
import           System.IO
import           Transaction

main :: IO ()
main = do
  putStrLn "Welcome to bank-app"
  putStrLn "-------------------"
  putStrLn "List of commands: "
  putStrLn $ unlines commands
  a <- loadBank
  b <- loop a
  return ()

prompt :: IO ()
prompt = do
  putStr "bank > "
  hFlush stdout

loop :: Bank -> IO Bank
loop b = runInputT defaultSettings (loop' b)
  where
    loop' :: Bank -> InputT IO Bank
    loop' b' = do
      (Just l) <- getInputLine "bank > "
      c <- doRun l
      (res, bank') <- doEvalCommand c b'
      let pend = pending $ transactions bank'
      mapM_ (lift . saveTransaction) pend
      let bank''= performPending bank'
      printResult (res, bank'')
      if shouldExit c
        then return bank''
        else loop' bank''

shouldExit :: Maybe Command -> Bool
shouldExit (Just QuitCmd) = True
shouldExit _              = False

printResult ::  (String, Bank) -> InputT IO()
printResult (s, _) = do
  outputStrLn "Result: "
  outputStrLn s
  return ()
