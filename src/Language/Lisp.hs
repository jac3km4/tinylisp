{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Lisp(runLisp, evalLisp, lisp) where
import           Control.Monad          (ap)
import           Control.Monad.Except   (MonadError (..))
import qualified Data.Char              as C
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Text              as T
import           Prelude
import           Relude.Extra.Foldable1 (foldl1')
import           Text.Read              (read)
import qualified Text.Show

type Symbol = Text

data PrimOp
  = List
  | Setq
  | Plus
  | Minus
  | Lt
  | Lambda
  | Defun
  | Print
  | If
  | Equals
  deriving (Show, Eq)

data Expr
  = Cell [Expr]
  | Symbol Symbol
  | Val Val
  deriving (Eq)

data Val
  = IntVal Int
  | TVal
  | Fun [Symbol] [Expr]
  | Prim PrimOp
  | Nil
  deriving (Eq)

instance Show Val where
  show (IntVal i) = show i
  show TVal = "t"
  show (Fun params body) = T.unpack $
    "(lambda (" <> T.intercalate " " params <> ") " <> T.intercalate " " (show <$> body) <> ")"
  show (Prim op) = show op
  show Nil = "nil"

instance Show Expr where
  show (Cell forms) = T.unpack $ "(" <> T.intercalate " " (show <$> forms) <> ")"
  show (Symbol sym) = T.unpack sym
  show (Val val) = show val

newtype Parser a = Parser { runParser :: Text -> Either Text (a, Text) }
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser $ \txt -> Right (a, txt)
  (<*>) = ap

instance Monad Parser where
  Parser fa >>= f = Parser $ \txt -> do
    (a, rem) <- fa txt
    (b, rem') <- runParser (f a) rem
    pure (b, rem')

instance Alternative Parser where
  empty = Parser $ \txt -> throwError $ "Unexpected input: " <> txt
  Parser fa <|> Parser fb = Parser (fa <> fb)

instance MonadError Text Parser where
  throwError e = Parser $ const $ Left e
  catchError (Parser fa) recover = join $ Parser $ \txt ->
    case fa txt of
      Left e         -> pure (recover e, txt)
      Right (a, rem) -> pure (pure a, rem)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = anyChar >>= parse
  where
    parse ch  | predicate ch = pure ch
    parse inp = throwError $ "Invalid input: " <> show inp

char :: Char -> Parser Char
char c = satisfy (== c)

notChar :: Char -> Parser Char
notChar c = satisfy (/= c)

anyChar :: Parser Char
anyChar = Parser $ maybe (throwError "Unexpected EOF") pure . T.uncons

comment :: Parser ()
comment = void $ char ';' *> many (notChar '\n') *> char '\n'

many1 :: Parser a -> Parser (NonEmpty a)
many1 fa = many fa >>= validate
  where
    validate res =
      maybe (throwError "Expected more than zero") pure $ nonEmpty res

text :: Text -> Parser Text
text txt = Parser $ \txt' ->
  case T.splitAt (T.length txt) txt' of
    (res, rem) | res == txt -> pure (res, rem)
    _ -> throwError $ "Got " <> txt' <> " when expected " <> txt

whitespace :: Parser ()
whitespace = void $ optional comment *> many (satisfy C.isSpace)

expr :: Parser Expr
expr = whitespace *> (int <|> bool <|> nil <|> symbol <|> list) <* whitespace
  where
    int = Val . IntVal . read . toList <$> many1 (satisfy (\c -> C.isDigit c || c == '-'))
    bool = Val TVal <$ char 't'
    nil = Val Nil <$ text "nil"
    exprs = toList <$> many1 expr
    dot = curry biList <$> expr <* char '.' <*> expr
    list = fmap Cell $ char '(' *> whitespace *> (dot <|> exprs) <* whitespace <* char ')'
    symbol =
        fmap (Symbol . T.pack) $ (:)
          <$> satisfy allowedChar
          <*> many (satisfy allowedChar <|> satisfy C.isDigit)
    allowedChar c = C.isAlpha c || isJust (T.findIndex (== c) "~!@#$%^&*-_=+:/?<>")

type Env = HashMap Symbol Val

newtype Lisp a = Lisp { unLisp :: StateT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadState Env)

bind :: Symbol -> Val -> Lisp Val
bind k v = modify' (HashMap.insert k v) $> v

eval :: Expr -> Lisp Val
eval (Cell (fn : args)) = eval fn >>= \case
  Fun params body -> do
    vals <- argvs >>= zipWithM bind params >> traverse eval body
    pure $ maybe Nil last . nonEmpty $ vals
  Prim op         -> evalPrim op
  _               -> fail $ "Must be function, got " <> show fn <> "!"
  where
    argvs = traverse eval args
    isT TVal = True
    isT _    = False
    expectInt (IntVal i) = pure i
    expectInt _          = fail "Expected int"
    expectSym (Symbol sym) = pure sym
    expectSym _            = fail "Expected symbol"
    evalPrim List = eval (Cell args)
    evalPrim Setq = do
      vals <- forM (chunksOf 2 args) $ \case
        (Symbol sym) : val : [] -> eval val >>= bind sym
        _ -> fail "Malformed setq"
      pure $ maybe Nil last $ nonEmpty vals
    evalPrim Plus =
      argvs >>= traverse expectInt <&> maybe Nil (IntVal . sum) . nonEmpty
    evalPrim Minus =
      argvs >>= traverse expectInt <&> maybe Nil (IntVal . foldl1' (-)) . nonEmpty
    evalPrim Lt = argvs >>= \case
      (IntVal lhs) : (IntVal rhs) : [] -> pure $ bool TVal Nil $ lhs < rhs
      _ -> fail "Malformed lt"
    evalPrim Equals = argvs >>= \case
      lhs : rhs : [] -> pure $ bool TVal Nil $ lhs == rhs
      _ -> fail "Malformed lt"
    evalPrim Lambda = case args of
      (Cell params) : body -> do
        params' <- traverse expectSym params
        pure $ Fun params' body
      _ -> fail "Malformed lambda"
    evalPrim Defun = case args of
      (Symbol name) : (Cell params) : body -> do
        params' <- traverse expectSym params
        bind name (Fun params' body)
      _ -> fail "Malformed defun"
    evalPrim Print = do
      argvs >>= traverse_ (liftIO . print)
      pure Nil
    evalPrim If =
      case args of
        cond : onT : onNil : [] ->
          eval cond >>= bool (eval onNil) (eval onT) . isT
        _ -> fail "Malformed if"
eval (Cell _) = pure Nil
eval (Val v) = pure v
eval (Symbol s) =
  gets (HashMap.lookup s) >>= maybe (fail "Unknown symbol") pure

lisp :: Text -> Lisp Val
lisp code =
  either (fail . T.unpack) (pure . fst) (runParser expr code) >>= eval

runLisp :: Lisp a -> IO a
runLisp ma = evalStateT (unLisp ma) initial

evalLisp :: Text -> IO Val
evalLisp code = evalStateT (unLisp (lisp code)) initial

initial :: Env
initial = HashMap.fromList
  [ ("+", Prim Plus)
  , ("-", Prim Minus)
  , ("<", Prim Lt)
  , ("eq", Prim Equals)
  , ("defun", Prim Defun)
  , ("lambda", Prim Lambda)
  , ("if", Prim If)
  , ("println", Prim Print)
  , ("setq", Prim Setq)
  ]

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (splitter ls (:) []) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n
