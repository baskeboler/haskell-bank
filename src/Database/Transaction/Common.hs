{-# LANGUAGE OverloadedStrings #-}

module Database.Transaction.Common where
import Database.SQLite.Simple


createTransactionsTable :: Query
createTransactionsTable = "create table if not exists transactions (id integer primary key)"


dropTransactionTable :: Query
dropTransactionTable = "drop table transactions"

insertTransaction ::  Connection -> Int -> IO()
insertTransaction conn _id = do
  execute conn "insert into transactions(id) values (?)" (Only (_id::Int))
  _ <- lastInsertRowId conn
  return ()
