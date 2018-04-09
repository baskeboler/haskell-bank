{-# LANGUAGE OverloadedStrings #-}
module Database.Account where
import Account
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.Config

data AccountFields = AccountFields Int T.Text Double deriving (Show)

instance FromRow AccountFields where
  fromRow = AccountFields <$> field <*> field <*> field

instance ToRow AccountFields where
  toRow (AccountFields _id name balance) = toRow (_id, name, balance)

toAccount :: AccountFields -> Account
toAccount (AccountFields _id name balance) = Account _id (T.unpack name) balance

fromAccount :: Account -> AccountFields
fromAccount (Account _id name balance) = AccountFields _id (T.pack name) balance

createAccountsTable :: Query
createAccountsTable = "CREATE TABLE IF NOT EXISTS accounts(id integer primary key, name text, balance real default 0.0)"

dropAccountsTable :: Query
dropAccountsTable = "drop table accounts"

insertAccount :: Query
insertAccount = "insert into accounts(id, name, balance) values (?, ?, ?)"

selectAccounts :: Query
selectAccounts = "select * from accounts"

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
    withBind stmt [_id::Int] $ nextRow stmt :: IO (Maybe AccountFields)
  close conn
  return $ fmap toAccount r

clearAccounts :: IO()
clearAccounts = do
  conn <- open dbFile
  execute_ conn "delete from accounts"
  close conn

saveAccounts :: [Account] -> IO()
saveAccounts accounts' = do
  clearAccounts
  conn <- open dbFile
  executeMany conn insertAccount $ map fromAccount accounts'
  close conn


