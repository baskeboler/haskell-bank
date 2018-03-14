module Parser where
  import           Control.Applicative
  import           Control.Monad
  import           Data.Char
  import Command

  newtype Parser a = Parser { parse :: String -> [(a, String)] }

  runParser :: Parser a -> String -> a
  runParser m s =
    case parse m s of
      [(res, [])] -> res
      [(_, rs)]   -> error "Parser did not consume entire stream"
      _           -> error "Parser error"

  tryParser :: Parser a -> String -> Maybe a
  tryParser m s =
    case parse m s of
      [(res, [])] -> Just res
      _           -> Nothing

  run :: String -> Maybe Command
  run = tryParser commandParser

  doRun :: String -> IO (Maybe Command)
  doRun s = return $ run s

  bind :: Parser a -> (a -> Parser b) -> Parser b
  bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

  unit :: a -> Parser a
  unit a = Parser (\s -> [(a, s)])

  instance Functor Parser where
    fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])
  instance Applicative Parser where
    pure = return
    (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])
  instance Monad Parser where
    return = unit
    (>>=) = bind
  instance MonadPlus Parser where
    mzero = failure
    mplus = combine

  instance Alternative Parser where
    empty = mzero
    (<|>) = option

  combine :: Parser a -> Parser a -> Parser a
  combine p q = Parser (\s -> parse p s ++ parse q s)

  failure :: Parser a
  failure = Parser (const [])

  option :: Parser a -> Parser a -> Parser a
  option p q = Parser $ \s ->
    case parse p s of
      []  -> parse q s
      res -> res

  characterItem :: Parser Char
  characterItem = Parser $ \s ->
    case s of
      []     -> []
      (c:cs) -> [(c, cs)]

  satisfy :: (Char -> Bool) -> Parser Char
  satisfy p = characterItem `bind` \c ->
    if p c then unit c else Parser (const [])

  oneOf :: [Char] -> Parser Char
  oneOf s = satisfy (`elem` s)

  chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
  chainl p op a = (p `chainl1` op) <|> return a

  chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
  p `chainl1` op = do { a <- p; rest a}
      where rest a = (do  f <- op
                          b <- p
                          rest (f a b)) <|>  return a

  char :: Char -> Parser Char
  char c = satisfy (c ==)

  natural :: Parser Integer
  natural = read <$> some (satisfy isDigit)

  string :: String -> Parser String
  string []     = return []
  string (c:cs) = do { char c; string cs; return (c:cs)}

  token :: Parser a -> Parser a
  token p = do { a <- p; spaces; return a}

  reserved :: String -> Parser String
  reserved s = token $ string s

  spaces :: Parser String
  spaces = many $ oneOf " \r\n"

  digit :: Parser Char
  digit = satisfy isDigit


  letter :: Parser Char
  letter = satisfy isLetter

  alphanumeric :: Parser Char
  alphanumeric = satisfy isAlphaNum

  word :: Parser String
  word = some letter

  identifier :: Parser String
  identifier = do
    a <- letter
    b <- some alphanumeric
    return (a : b)

  number :: Parser Int
  number = do
    s <- string "-" <|> return []
    cs <- some digit
    return $ read (s ++ cs)


  real :: Parser Double
  real = do
    s <- string "-" <|> return []
    cs <- some digit
    cs2 <- do {
      dot <- string ".";
      cs2 <- some digit;
      return $ dot ++ cs2
    } <|> return []
    return $ read (s ++ cs ++ cs2)

  createAccountCmd :: Parser Command
  createAccountCmd = do
    reserved "create"
    reserved "account"
    name <- token identifier
    return (CreateAccountCmd name)

  depositCmd :: Parser Command
  depositCmd = do
    reserved "deposit"
    acc <- token number
    amount <- token real
    return (DepositCmd acc amount)

  withdrawalCmd :: Parser Command
  withdrawalCmd = do
    reserved "withdraw"
    a <- token number
    amount <- token real
    return (WidthdrawCmd a amount)

  transferCmd :: Parser Command
  transferCmd = do
    reserved "transfer"
    a <- token number
    b <- token number
    amount <- token real
    return (TransferCmd a b amount)

  showTransactionsCmd :: Parser Command
  showTransactionsCmd = do
    reserved "status"
    a <- token number
    return (ShowTransactionsCmd a)

  quitCmd :: Parser Command
  quitCmd = do
    reserved "quit"
    return QuitCmd
  accountsCmd :: Parser Command
  accountsCmd = do
    reserved "accounts"
    return AccountsCmd

  commandParser :: Parser Command
  commandParser = createAccountCmd
    <|> depositCmd
    <|> withdrawalCmd
    <|> transferCmd
    <|> showTransactionsCmd
    <|> quitCmd
    <|> accountsCmd
