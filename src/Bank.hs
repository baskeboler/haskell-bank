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

  withAccounts :: [Account] -> Bank -> Bank
  withAccounts accs (Bank i j _ txns) = Bank i j accs txns

  withTransactions :: Transactions -> Bank -> Bank
  withTransactions txns (Bank i j accs _) = Bank i j accs txns

  withNewAccount :: Bank -> Bank
  withNewAccount (Bank i j a b) = Bank (i+1) j a b

  withNewTransaction :: Bank -> Bank
  withNewTransaction (Bank i j a b) = Bank i (j+1) a b

  addAccount :: Account -> Bank -> Bank
  addAccount a bank = withAccounts (a : accounts bank) bank

  createAccount :: String -> Bank -> (Account, Bank)
  createAccount name bank = (acc', bank')
    where
      acc' = Account.createAccount (nextAccountId bank) name
      bank' = addAccount acc' (withNewAccount bank)



  getAccount :: AccountId -> Bank -> Maybe Account
  getAccount id_ bank = find' id_ $ accounts bank
    where find' i [] = Nothing
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
  addTransaction t@(Deposit _ acc n) b =
    getAccount (getId acc) b >> return (withTransactions (addPending t (transactions b)) b)
  addTransaction t@(Withdrawal _ acc n) b =
    getAccount (getId acc) b >> return (withTransactions (addPending t (transactions b)) b)
  addTransaction t@(Transfer _ from to n) b =
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
  performTransaction t bank = performTransaction' t bank
    >>= \b -> return (recordTransaction t b)

  performTransaction' t@(Deposit _ acc n) bank =
    getAccount (getId acc) bank
      >>= \a -> ((\a' -> return (updateAccount a' bank)) (deposit a n))
  performTransaction' t@(Withdrawal _ acc n) bank =
    getAccount (getId acc) bank
      >>= \a -> withdraw a n
      >>= \a' -> return (updateAccount a' bank)
  performTransaction' t@(Transfer _ from to n) bank =
    getAccount (getId from) bank
      >>= \a -> withdraw a n
      >>= \a'-> return (a', deposit to n)
      >>= \(a'', b) -> return (updateAccount a'' (updateAccount b bank))

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
