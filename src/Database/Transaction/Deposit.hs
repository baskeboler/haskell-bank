{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Transaction.Deposit where
import Database.SQLite.Simple
import Bank
import Transaction
import Account 
import Database.Config
import Database.Transaction.Common
import Data.Maybe


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
createDepositsTable = "create table if not exists deposits (id integer primary key, account_id integer not null, amount real not null default 0.0, foreign key (id) references transactions(id) on delete cascade, foreign key(account_id) references accounts (id))"


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
  let items = map (`toDeposit` bank) r
  return $ map fromJust $ filter (/= Nothing) items

saveDeposit :: Transaction -> IO()
saveDeposit  txn@(Deposit _id _ _) = do
  conn <- open dbFile
  withTransaction conn $ do
    insertTransaction conn _id
    execute conn insertDepositQuery $ fromDeposit txn
  close conn
saveDeposit _ = error "not a deposit"

saveDeposits :: [Transaction] -> IO()
saveDeposits txs = do
  _ <- mapM_ saveDeposit txs
  return ()

