{-# LANGUAGE OverloadedStrings #-}
module Database where

import           Bank
import           Data.List                       (sortOn)
import           Database.Account
import           Database.Config
import           Database.SQLite.Simple
import           Database.Transaction.Common
import           Database.Transaction.Deposit
import           Database.Transaction.Transfer
import           Database.Transaction.Withdrawal
import           Transaction
createTables :: IO()
createTables = do
  conn <- open dbFile
  execute_ conn createAccountsTable
  execute_ conn createTransactionsTable
  execute_ conn createWithdrawalsTable
  execute_ conn createDepositsTable
  execute_ conn createTransfersTable
  close conn

dropTables :: IO()
dropTables = do
  conn <- open dbFile
  execute_ conn dropTransfersTable
  execute_ conn dropDepositsTable
  execute_ conn dropWithdrawalsTable
  execute_ conn dropTransactionTable
  execute_ conn dropAccountsTable
  close conn




loadBank :: IO Bank
loadBank = do
  conn <- open dbFile
  [[lastTxnId]]  <- query_ conn "select case when max(id) is NULL then 1 else max(id) + 1 end from transactions" :: IO [[Int]]
  [[lastAccountId]] <- query_ conn "select case when max(id) is null then  1 else max(id) + 1 end from accounts" :: IO [[Int]]
  close conn
  accounts' <- loadAccountsFromDb
  let b = withNextTransationId lastTxnId $
        withNextAccountId lastAccountId $
        withAccounts accounts' newBank
  withdrawals' <- loadWithdrawalsFromDb b
  deposits' <- loadDepositsFromDb b
  transfers' <- loadTransfersFromDb b
  let b' = withTransactions (newTransactions_ [] (sortOn transactionId (withdrawals'++deposits'++transfers')) []) b
  return b'

saveBank :: Bank -> IO()
saveBank bank = do
  conn <- open dbFile
  _ <- execute_ conn "delete from withdrawals"
  _ <- execute_ conn "delete from deposits"
  _ <- execute_ conn "delete from transfers"
  _ <- execute_ conn "delete from transactions"
  close conn
  saveAccounts (accounts bank)
  let wds = filterWithdrawals (transactions bank)
  saveWithdrawals wds
  let deps = filterDeposits (transactions bank)
  saveDeposits deps
  let trxs = filterTransfers (transactions bank)
  saveTransfers trxs
  saveAccounts (accounts bank)

saveTransaction :: Transaction -> IO()
saveTransaction txn@Deposit{}    = saveDeposit txn
saveTransaction txn@Withdrawal{} = saveWithdrawal txn
saveTransaction txn@Transfer{}   = saveTransfer txn
