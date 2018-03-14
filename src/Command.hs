module Command where
import Bank
import Data.Maybe
import Control.Monad

data Command =
  CreateAccountCmd String
  | DepositCmd Int Double
  | WidthdrawCmd Int Double
  | TransferCmd Int Int Double
  | ShowTransactionsCmd Int
  | QuitCmd deriving (Eq, Show)

evalCommand :: Command -> Bank -> (String, Bank)

evalCommand (CreateAccountCmd name) bank =  (show acc, bank')
  where (acc, bank') = createAccount name bank
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

evalCommand _ bank = ("Whatever.", bank)

doEvalCommand :: Maybe Command -> Bank -> IO(String, Bank)
doEvalCommand Nothing bank = return ("Error", bank)
doEvalCommand (Just c) b = return $ evalCommand c b
