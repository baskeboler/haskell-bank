module Lib where
  import Transaction
  import Account
  someFunc :: IO ()
  someFunc = putStrLn "someFunc"

  txn1 :: Transaction
  txn1 = Withdrawal 0 testAccount1 15

  txn2 :: Transaction
  txn2 = Transfer 1 testAccount1 testAccount2 25
