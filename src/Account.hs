module Account where

type AccountId = Int
data Account = Account AccountId String Double
  deriving (Eq, Show)

createAccount :: AccountId -> String -> Account
createAccount id_ name = Account id_ name 0

deposit :: Account -> Double -> Account
deposit (Account id_ name total) n = Account id_ name (total + n)

withdraw :: Account -> Double -> Maybe Account
withdraw (Account id_ name total) n | n > total = Nothing
  | otherwise = Just $ Account id_ name (total - n)

transfer :: Account -> Account -> Double -> Maybe (Account, Account)
transfer from to amount = withdraw from amount >>= \f -> return (f, deposit to amount)

getId :: Account -> AccountId
getId (Account id_ _ _) = id_

testAccount1 :: Account
testAccount1 = Account 0 "victor" 100
testAccount2 :: Account
testAccount2 = Account 1 "ramon" 150
