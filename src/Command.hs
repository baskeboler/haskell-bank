module Command where
import Bank
import Account
import Data.Maybe
import Control.Monad

data Command =
  CreateAccountCmd String
  | DepositCmd Int Double
  | WidthdrawCmd Int Double
  | TransferCmd Int Int Double
  | ShowTransactionsCmd Int
  | QuitCmd
  | AccountsCmd
    deriving (Eq, Show)

evalCommand :: Command -> Bank -> (String, Bank)

evalCommand (CreateAccountCmd name) bank =  (show acc, bank')
  where (acc, bank') = Bank.createAccount name bank
evalCommand (DepositCmd id_ amount) bank = (show acc, bank')
  where (acc, bank') =
          maybe ("Failed", bank)
                (\(t, b) -> ("OK", b))
                (addDeposit id_ amount bank)
evalCommand (WidthdrawCmd id_ amount) bank = (show acc, bank')
  where (acc, bank') =
          maybe ("Failed", bank)
                (\(t, b) -> ("OK", b))
                (addWithdrawal id_ amount bank)
evalCommand (TransferCmd from_ to_ amount) bank = (show acc, bank')
  where (acc, bank') =
          maybe ("Failed", bank)
                (\(t, b) -> ("OK", b))
                (addTransfer from_ to_ amount bank)
evalCommand (ShowTransactionsCmd id_) bank = (completed' ++ invalid', bank)
  where completed' = unlines ("list of transactions" : map show txns)
        txns = getCompletedTransactions id_ bank
        invalid = getInvalidTransactions id_ bank
        invalid' = if not (null invalid)
          then unlines ("" : "list of rejected transacions" : map show invalid)
          else ""

evalCommand AccountsCmd bank = (status, bank)
  where status = unlines $ map formatAccount $ accounts bank
        formatAccount (Account id_ name total) = show id_ ++ " - name: " ++ name ++ ", available: " ++ show total
evalCommand _ bank = ("Whatever.", bank)

commands :: [String]
commands = ["create account <name> - creates a new account",
  "deposit <account> <amount> - deposits amount into account",
  "withdraw <account> <amount> - withdraws amount from account",
  "transfer <from> <to> <amount> - transfers amount from from to to",
  "status <account> - lists txns for account",
  "accounts - lists all accounts",
  "quit - quits the application"]

doEvalCommand :: Maybe Command -> Bank -> IO(String, Bank)
doEvalCommand Nothing bank = return (unlines commands, bank)
doEvalCommand (Just c) b = return $ evalCommand c b
