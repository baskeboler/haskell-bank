{-# LANGUAGE OverloadedStrings #-}
module Database where

import           Account
import           Bank
import           Control.Applicative
import           Data.Maybe
import qualified Data.Text                      as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Transaction

data AccountFields = AccountFields Int T.Text Double deriving (Show)

instance FromRow AccountFields where
  fromRow = AccountFields <$> field <*> field <*> field

instance ToRow AccountFields where
  toRow (AccountFields _id name balance) = toRow (_id, name, balance)

toAccount :: AccountFields -> Account
toAccount (AccountFields _id name balance) = Account _id (T.unpack name) balance

fromAccount :: Account -> AccountFields
fromAccount (Account _id name balance) = AccountFields _id (T.pack name) balance

dbFile :: String
dbFile = "bank.db"

createAccountsTable :: Query
createAccountsTable = "CREATE TABLE IF NOT EXISTS accounts(id integer primary key, name text, balance real default 0.0)"

dropAccountsTable :: Query
dropAccountsTable = "drop table accounts"

insertAccount :: Query
insertAccount = "insert into accounts(id, name, balance) values (?, ?, ?)"

selectAccounts :: Query
selectAccounts = "select * from accounts"

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

loadAccountsFromDb :: IO [Account]
loadAccountsFromDb = do
  conn <- open dbFile
  r <- query_ conn selectAccounts :: IO [AccountFields]
  close conn
  let items = map toAccount r
  return items

getAccount :: Int -> IO (Maybe Account)
getAccount _id = do
  conn <- open dbFile
  r <- withStatement conn "select * from accounts where id = ?" $ \stmt ->
    withBind stmt [_id::Int] $ do
      row <- nextRow stmt :: IO (Maybe AccountFields)
      return row
  close conn
  return $ maybe Nothing (Just . toAccount) r

clearAccounts :: IO()
clearAccounts = do
  conn <- open dbFile
  execute_ conn "delete from accounts"
  close conn

saveAccounts :: [Account] -> IO()
saveAccounts accounts = do
  clearAccounts
  conn <- open dbFile
  executeMany conn insertAccount $ map fromAccount accounts
  close conn


data TransactionFields = TransactionFields Int deriving (Show)
data WithdrawalFields = WithdrawalFields Int Int Double deriving (Show)

instance FromRow WithdrawalFields where
  fromRow = WithdrawalFields <$> field <*> field <*> field

instance ToRow WithdrawalFields where
  toRow (WithdrawalFields _id _accountId amount) = toRow (_id, _accountId, amount)

toWithdrawal :: WithdrawalFields -> Bank -> Maybe Transaction
toWithdrawal (WithdrawalFields _id _accountId _amount) bank = do
  account <- Bank.getAccount _accountId bank
  return $ Withdrawal _id account _amount

fromWithdrawal :: Transaction -> WithdrawalFields
fromWithdrawal (Withdrawal _id (Account _accountId _ _) amount) =
  WithdrawalFields _id _accountId amount
fromWithdrawal _ = error "not a withdrawal"

createTransactionsTable :: Query
createTransactionsTable = "create table if not exists transactions (id integer primary key)"

{- Withdrawals -}
createWithdrawalsTable :: Query
createWithdrawalsTable = "create table if not exists withdrawals (id integer primary key, account_id integer not null, amount real not null default 0.0, foreign key (id) references transactions(id), foreign key(account_id) references accounts (id))"


dropTransactionTable :: Query
dropTransactionTable = "drop table transactions"

dropWithdrawalsTable :: Query
dropWithdrawalsTable = "drop table withdrawals"


insertWithdrawalQuery :: Query
insertWithdrawalQuery = "insert into withdrawals(id, account_id, amount) values (?, ?, ?)"

selectWithdrawals :: Query
selectWithdrawals = "select * from withdrawals"

loadWithdrawalsFromDb :: Bank -> IO [Transaction]
loadWithdrawalsFromDb bank = do
  conn <- open dbFile
  r <- query_ conn selectWithdrawals :: IO [WithdrawalFields]
  close conn
  let items = map (flip toWithdrawal bank) r
  return $ map fromJust $ filter (/= Nothing) items

saveWithdrawal :: Transaction -> IO()
saveWithdrawal txn@(Withdrawal _id _ _) = do
  conn <- open dbFile
  execute conn "insert into transactions(id) values (?)" (Only (_id::Int))
  _ <- lastInsertRowId conn
  execute conn insertWithdrawalQuery $ fromWithdrawal txn
  close conn
saveWithdrawal _ = error "not a withdrawal"

saveWithdrawals :: [Transaction] -> IO ()
saveWithdrawals txs =  do
  _ <- mapM saveWithdrawal txs
  return ()

{- Deposits -}


data DepositFields = DepositFields Int Int Double deriving (Show)

instance FromRow DepositFields where
  fromRow = DepositFields <$> field <*> field <*> field

instance ToRow DepositFields where
  toRow (DepositFields _id _accountId amount) = toRow (_id, _accountId, amount)

toDeposit :: DepositFields -> Bank -> Maybe Transaction
toDeposit (DepositFields _id _accountId _amount) bank = do
  account <- Bank.getAccount _accountId bank
  return $ Deposit _id account _amount

fromDeposit :: Transaction -> DepositFields
fromDeposit (Deposit _id (Account _accountId _ _) amount) =
  DepositFields _id _accountId amount
fromDeposit _ = error "not a deposit"



createDepositsTable :: Query
createDepositsTable = "create table if not exists deposits (id integer primary key, account_id integer not null, amount real not null default 0.0, foreign key (id) references transactions(id), foreign key(account_id) references accounts (id))"


dropDepositsTable :: Query
dropDepositsTable = "drop table deposits"


insertDepositQuery :: Query
insertDepositQuery = "insert into deposits(id, account_id, amount) values (?, ?, ?)"

selectDeposits :: Query
selectDeposits = "select * from deposits"

loadDepositsFromDb :: Bank -> IO [Transaction]
loadDepositsFromDb bank = do
  conn <- open dbFile
  r <- query_ conn selectDeposits :: IO [DepositFields]
  close conn
  let items = map (flip toDeposit bank) r
  return $ map fromJust $ filter (/= Nothing) items

saveDeposit :: Transaction -> IO()
saveDeposit  txn@(Deposit _id _ _) = do
  conn <- open dbFile
  execute conn "insert into transactions(id) values (?)" (Only (_id::Int))
  _ <- lastInsertRowId conn
  execute conn insertDepositQuery $ fromDeposit txn
  close conn
saveDeposit _ = error "not a deposit"

saveDeposits :: [Transaction] -> IO()
saveDeposits txs = do
  _ <- mapM saveDeposit txs
  return ()

{- Transfers -}


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
  let items = map (flip toTransfer bank) r
  return $ map fromJust $ filter (/= Nothing) items

saveTransfer :: Transaction -> IO()
saveTransfer  txn@(Transfer _id _ _ _) = do
  conn <- open dbFile
  execute conn "insert into transactions(id) values (?)" (Only (_id::Int))
  _ <- lastInsertRowId conn
  execute conn insertTransferQuery $ fromTransfer txn
  close conn
saveTransfer _ = error "not a transfer"

saveTransfers :: [Transaction] -> IO()
saveTransfers txs = do
  _ <- mapM saveTransfer txs
  return ()


{- Bank -}

loadBank :: IO Bank
loadBank = do
  conn <- open dbFile
  [[lastTxnId]]  <- query_ conn "select max(id) + 1 from transactions" :: IO [[Int]]
  print "trx id: "
  print lastTxnId
  [[lastAccountId]] <- query_ conn "select max(id) + 1 from accounts" :: IO [[Int]]
  accounts' <- loadAccountsFromDb
  let b = withNextTransationId lastTxnId $
        withNextAccountId lastAccountId $
        withAccounts accounts' newBank
  withdrawals' <- loadWithdrawalsFromDb b
  deposits' <- loadDepositsFromDb b
  transfers' <- loadTransfersFromDb b
  let b' = withTransactions (newTransactions_ [] (withdrawals'++deposits'++transfers') []) b
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
