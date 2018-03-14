module Transaction where
  import Account

  type TransactionId = Int


  data Transaction =
    Withdrawal TransactionId Account Double
    | Deposit TransactionId Account Double
    | Transfer TransactionId Account Account Double
    deriving (Eq, Show)

  data Transactions = Transactions {
    pending :: [Transaction],
    completed :: [Transaction],
    invalid :: [Transaction]
  } deriving (Eq, Show)

  addPending :: Transaction -> Transactions -> Transactions
  addPending t (Transactions p c i) = Transactions (t:p) c i

  addCompleted :: Transaction -> Transactions -> Transactions
  addCompleted t (Transactions p c i) = Transactions p (t:c) i

  addInvalid :: Transaction -> Transactions -> Transactions
  addInvalid t (Transactions p c i) = Transactions p c (t:i)

  nextPendingTransaction :: Transactions -> Maybe (Transaction, Transactions)
  nextPendingTransaction (Transactions [] _ _) = Nothing
  nextPendingTransaction (Transactions (x:xs) c i) = Just (x, Transactions xs c i)
  

  newTransactions :: Transactions
  newTransactions = Transactions [] [] []
