module Bank where
  import Account
  import Transaction
  import Data.Maybe

  data Bank = Bank {
    nextAccountId :: AccountId,
    nextTransactionId :: TransactionId,
    accounts :: [Account],
    transactions :: Transactions
  }  deriving (Eq, Show)

  recordTransaction :: Transaction -> Bank -> Bank
  recordTransaction t bank =
    withTransactions (addCompleted t (transactions bank)) bank

  newBank :: Bank
  newBank = Bank 0 0 [] newTransactions

  withNextAccountId :: AccountId -> Bank -> Bank
  withNextAccountId _id (Bank _ j accs txns) = Bank _id j accs txns

  withNextTransationId :: TransactionId -> Bank -> Bank
  withNextTransationId _id (Bank i _ accs txns) = Bank i _id accs txns

  withAccounts :: [Account] -> Bank -> Bank
  withAccounts accs (Bank i j _ txns) = Bank i j accs txns

  withTransactions :: Transactions -> Bank -> Bank
  withTransactions txns (Bank i j accs _) = Bank i j accs txns

  withNewAccount :: Bank -> Bank
  withNewAccount (Bank i j a b) = Bank (i+1) j a b

  withNewTransaction :: Bank -> Bank
  withNewTransaction (Bank i j a b) = Bank i (j+1) a b

  performedBy :: AccountId -> Transaction -> Bool
  performedBy i (Deposit _ i' _) = i == getId i'
  performedBy i (Withdrawal _ i' _) = i == getId i'
  performedBy i (Transfer _ i' i'' _) = i == getId i' || i == getId i''
  -- performedBy _ _ = False

  getCompletedTransactions :: AccountId -> Bank -> [Transaction]
  getCompletedTransactions id_ b = txns
    where txns' = transactions b
          completed' = completed txns'
          txns = filter (performedBy id_) completed'

  getInvalidTransactions :: AccountId -> Bank -> [Transaction]
  getInvalidTransactions id_ b = txns
    where txns' = transactions b
          invalid'= invalid txns'
          txns = filter (performedBy id_) invalid'

  addAccount :: Account -> Bank -> Bank
  addAccount a bank = withAccounts (a : accounts bank) bank

  createAccount :: String -> Bank -> (Account, Bank)
  createAccount name bank = (acc', bank')
    where
      acc' = Account.createAccount (nextAccountId bank) name
      bank' = addAccount acc' (withNewAccount bank)



  getAccount :: AccountId -> Bank -> Maybe Account
  getAccount id_ bank = find' id_ $ accounts bank
    where find' _ [] = Nothing
          find' i (a@(Account i' _ _):xs) =
            if i == i'
              then Just a
              else find' i xs

  updateAccount :: Account -> Bank -> Bank
  updateAccount acc bank = withAccounts (map (\a ->
    if getId a == getId acc
      then acc
      else a) (accounts bank)) bank

  addTransaction :: Transaction -> Bank -> Maybe Bank
  addTransaction t@(Deposit _ acc _) b =
    getAccount (getId acc) b >> return (withTransactions (addPending t (transactions b)) b)
  addTransaction t@(Withdrawal _ acc _) b =
    getAccount (getId acc) b >> return (withTransactions (addPending t (transactions b)) b)
  addTransaction t@(Transfer _ from to _) b =
    getAccount (getId from) b
      >> getAccount (getId to) b
      >> return (withTransactions (addPending t (transactions b)) b)
      -- >> return (Bank accs (addPending t txns))

  addDeposit :: AccountId -> Double -> Bank -> Maybe (Transaction, Bank)
  addDeposit id_ n bank = do
    a <- getAccount id_ bank
    let t  =  Deposit (nextTransactionId bank) a n
    let bank' = withNewTransaction bank
    return (t, withTransactions (addPending t (transactions bank')) bank')

  addWithdrawal :: AccountId -> Double -> Bank -> Maybe (Transaction, Bank)
  addWithdrawal id_ n bank = do
    a <- getAccount id_ bank
    let t = Withdrawal (nextTransactionId bank) a n
    let bank' = withNewTransaction bank
    return (t, withTransactions (addPending t (transactions bank')) bank')

  addTransfer :: AccountId -> AccountId -> Double -> Bank -> Maybe (Transaction, Bank)
  addTransfer from_ to_ n bank = do
    a <- getAccount from_ bank
    b <- getAccount to_ bank
    let t = Transfer (nextTransactionId bank) a b n
    let bank' = withNewTransaction bank
    return (t, withTransactions (addPending t (transactions bank')) bank')

  performTransaction :: Transaction -> Bank -> Maybe Bank
  performTransaction t bank = performTransaction2 t bank
    >>= \b -> return (recordTransaction t b)
{-
  performTransaction' :: Transaction -> Bank -> Maybe Bank
  performTransaction' (Deposit _ acc n) bank =
    getAccount (getId acc) bank
      >>= \a -> ((\a' -> return (updateAccount a' bank)) (deposit a n))
  performTransaction' (Withdrawal _ acc n) bank =
    getAccount (getId acc) bank
      >>= \a -> withdraw a n
      >>= \a' -> return (updateAccount a' bank)
  performTransaction' (Transfer _ from to n) bank =
    getAccount (getId from) bank
      >>= \a -> withdraw a n
      >>= \a'-> return (a', deposit to n)
      >>= \(a'', b) -> return (updateAccount a'' (updateAccount b bank))
-}

  performTransaction2 :: Transaction -> Bank -> Maybe Bank
  performTransaction2 (Deposit _ acc n) bank = do
    a <- getAccount (getId acc) bank
    let a' = deposit a n
    return $ updateAccount a' bank
  performTransaction2 (Withdrawal _ acc n) bank = do
    a <- getAccount (getId acc) bank
    a' <- withdraw a n
    return $ updateAccount a' bank
  performTransaction2 (Transfer _ from to n) bank = do
    fromAccount <- getAccount (getId from) bank
    fromAccount' <- withdraw fromAccount n
    toAccount <- getAccount (getId to) bank
    let toAccount' = deposit toAccount n
    return $ updateAccount fromAccount' $ updateAccount toAccount' bank


  performPending :: Bank -> Bank
  performPending bank = fromMaybe bank maybeBank
    where
      txns = transactions bank
      maybeBank = do
        (t, txns') <- nextPendingTransaction txns
        return $
            fromMaybe (withTransactions (addInvalid t txns') bank) $
                performTransaction t (withTransactions txns' bank)

  testBank :: Bank
  testBank = withAccounts [testAccount1, testAccount2] $ withNewAccount $ withNewAccount newBank
