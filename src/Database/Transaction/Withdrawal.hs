{-# LANGUAGE OverloadedStrings #-}
module Database.Transaction.Withdrawal where
import           Account
import           Bank
import qualified Data.Text              as T
import           Database.SQLite.Simple
import           Transaction
import Database.Config
import Database.Transaction.Common
import Data.Maybe

-- newtype TransactionFields = TransactionFields Int deriving (Show)
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

createWithdrawalsTable :: Query
createWithdrawalsTable = "create table if not exists withdrawals (id integer primary key, account_id integer not null, amount real not null default 0.0, foreign key (id) references transactions(id) on delete cascade, foreign key(account_id) references accounts (id))"

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
  let items = map (`toWithdrawal` bank) r
  return $ map fromJust $ filter (/= Nothing) items

saveWithdrawal :: Transaction -> IO()
saveWithdrawal txn@(Withdrawal _id _ _) = do
  conn <- open dbFile
  withTransaction conn $ do
     insertTransaction conn _id
     execute conn insertWithdrawalQuery $ fromWithdrawal txn
     return ()
  close conn
saveWithdrawal _ = error "not a withdrawal"

saveWithdrawals :: [Transaction] -> IO ()
saveWithdrawals txs =  do
  _ <- mapM_ saveWithdrawal txs
  return ()

