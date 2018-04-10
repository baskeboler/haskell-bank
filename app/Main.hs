module Main where

import           Bank
import           Command
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Maybe
import           Database
import           Database.Account
import           Parser
import           System.Console.Haskeline
import           Transaction

main :: IO ()
main = do
  putStrLn "Welcome to bank-app"
  putStrLn "-------------------"
  putStrLn "List of commands: "
  putStrLn $ unlines commands
  a <- loadBank
  _ <- loop4 a
  return ()

type Input = InputT IO
type Output = Maybe Command
type MainLoop = StateT Bank Input Output

loop2 :: MainLoop
loop2 = do
  bank <- get
  l <- lift $ getInputLine "bank > "
  c <- lift $ doRun $ fromJust l
  (res, bank') <- lift $ doEvalCommand c bank
  let pend = pending $ transactions bank'
  mapM_ (lift . lift . saveTransaction) pend
  put bank'
  let bank'' = performPending bank'
  put bank''
  lift $ printResult (res, bank')
  if shouldExit c
    then
      do
        lift $ outputStrLn "Leaving."
        b'' <- get
        (lift . lift . saveAccounts) (accounts b'')
        lift $ outputStrLn "Should be finished saving accounts"
        return c
    else loop2

loop4 :: Bank -> IO Output
loop4 b = runInputT defaultSettings (loop' b)
  where loop' :: Bank -> Input Output
        loop' = evalStateT loop2

shouldExit :: Output -> Bool
shouldExit (Just QuitCmd) = True
shouldExit _              = False

printResult ::  (String, Bank) -> InputT IO()
printResult (s, _) = do
  outputStrLn "Result: "
  outputStrLn s
  return ()
