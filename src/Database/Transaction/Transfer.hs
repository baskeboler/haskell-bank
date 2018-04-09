{-# LANGUAGE OverloadedStrings #-}
module Database.Transaction.Transfer where
import Database.SQLite.Simple
import Bank
import Transaction
import Account
import Database.Config
import Database.Transaction.Common
import Data.Maybe


data TransferFields = TransferFields Int Int Int Double deriving (Show)

instance FromRow TransferFields where
  fromRow = TransferFields <$> field <*> field <*> field <*> field

instance ToRow TransferFields where
  toRow (TransferFields _id _accountIdFrom _accountIdTo amount) = toRow (_id, _accountIdFrom, _accountIdTo, amount)

toTransfer :: TransferFields -> Bank -> Maybe Transaction
toTransfer (TransferFields _id _accountIdFrom _accountIdTo _amount) bank = do
  accountFrom <- Bank.getAccount _accountIdFrom  bank
  accountTo <- Bank.getAccount _accountIdTo bank
  return $ Transfer _id accountFrom accountTo _amount

fromTransfer :: Transaction -> TransferFields
fromTransfer (Transfer _id (Account _accountIdFrom _ _) (Account _accountIdTo _ _)amount) =
  TransferFields _id _accountIdFrom _accountIdTo amount
fromTransfer _ = error "not a transfer"



createTransfersTable :: Query
createTransfersTable = "create table if not exists transfers (id integer primary key, account_from_id integer not null, account_to_id integer not null, amount real not null default 0.0, foreign key (id) references transactions(id), foreign key(account_from_id) references accounts (id), foreign key(account_to_id) references accounts(id))"


dropTransfersTable :: Query
dropTransfersTable = "drop table transfers"


insertTransferQuery :: Query
insertTransferQuery = "insert into transfers(id, account_from_id, account_to_id, amount) values (?, ?, ?, ?)"

selectTransfers :: Query
selectTransfers = "select * from transfers"

loadTransfersFromDb :: Bank -> IO [Transaction]
loadTransfersFromDb bank = do
  conn <- open dbFile
  r <- query_ conn selectTransfers :: IO [TransferFields]
  close conn
  let items = map (`toTransfer`  bank) r
  return $ map fromJust $ filter (/= Nothing) items

saveTransfer :: Transaction -> IO()
saveTransfer  txn@(Transfer _id _ _ _) = do
  conn <- open dbFile
  insertTransaction conn _id
  execute conn insertTransferQuery $ fromTransfer txn
  close conn
saveTransfer _ = error "not a transfer"

saveTransfers :: [Transaction] -> IO()
saveTransfers txs = do
  _ <- mapM_ saveTransfer txs
  return ()

