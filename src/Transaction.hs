module Transaction where
  import Account

  type TransactionId = Int


  data Transaction =
    Withdrawal TransactionId Account Double
    | Deposit TransactionId Account Double
    | Transfer TransactionId Account Account Double
    deriving (Eq)

  instance Show Transaction where
    show (Withdrawal i a amount)  =
      "[" ++ show i ++ "] [withdrawal] account: "
        ++ show (getId a) ++ ", amount: " ++ show amount
    show (Deposit i a amount)  =
      "[" ++ show i ++ "] [deposit] account: "
        ++ show (getId a) ++ ", amount: " ++ show amount
    show (Transfer i a b amount)  =
      "[" ++ show i ++ "] [transfer] from: "
        ++ show (getId a) ++ ", to: " ++ show (getId b)
        ++ ", amount: " ++ show amount

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

  newTransactions_ :: [Transaction] -> [Transaction] -> [Transaction] -> Transactions
  newTransactions_ a b c = Transactions a b c

  filterWithdrawals :: Transactions -> [Transaction]
  filterWithdrawals txns = filter isWithdrawal (completed txns)
     where isWithdrawal (Withdrawal _ _ _) = True
           isWithdrawal _ = False

  filterDeposits :: Transactions -> [Transaction]
  filterDeposits txns = filter isDeposit (completed txns)
     where isDeposit (Deposit _ _ _) = True
           isDeposit _ = False

  filterTransfers :: Transactions -> [Transaction]
  filterTransfers txns = filter isTransfer (completed txns)
     where isTransfer (Transfer _ _ _ _) = True
           isTransfer _ = False
