{-# LANGUAGE FlexibleContexts #-}

module Chapter1 where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Log
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter
import Snail
import System.IO (stdout)
import Text.Read (readMaybe)

-- | TODO: Unsure what this is used for yet
newtype Info = Info SnailAst
  deriving stock (Show)

data LangError
  = TextLiteralUnsupported
  | EmptyExpression
  | UnknownLexeme Text
  deriving stock (Show)

{- | The AST for $\mathcal{L}_\mathrm{int}$

Both 'AstInt' and 'Read' are leaves of 'Ast', they don't take 'Ast' as an
argument.
-}
data Ast
  = AstInt Integer
  | Read
  | Program Info Ast
  | Operation Text [Ast]
  | Negate Ast
  deriving stock (Show)

unwrap :: SnailAst -> SnailAst
unwrap = \case
  SExpression _ [x] -> x
  x -> x

flatten :: [SnailAst] -> [SnailAst]
flatten = \case
  [] -> []
  (x : rest) -> unwrap x : (unwrap <$> rest)

parseLeaf :: (MonadError LangError m) => Text -> m Ast
parseLeaf = \case
  "read" -> pure Read
  txt ->
    case readMaybe @Integer $ Text.unpack txt of
      Nothing -> throwError $ UnknownLexeme txt
      Just int -> pure $ AstInt int

logSnailAst :: (MonadLog (WithSeverity (Doc ann)) m) => Text -> SnailAst -> m ()
logSnailAst msg expr = logInfo . pretty $ msg <> ": " <> toText expr

fromSnail :: (MonadLog (WithSeverity (Doc ann)) m, MonadError LangError m) => SnailAst -> m Ast
fromSnail = \case
  -- `X` where `X` is a leaf in 'Ast'
  Lexeme (_, leaf) -> parseLeaf leaf
  -- no text literals are supported in this language
  TextLiteral _ -> throwError TextLiteralUnsupported
  -- `(- X)` where X is an integer or an S-expression
  SExpression _ [Lexeme (_, op@"-"), arg] -> do
    logSnailAst "Op -" arg
    operand <- fromSnail arg
    pure $ Operation op [Negate operand]
  -- `(+ X Y)` where X and Y are an integer or an S-expression
  SExpression _ [Lexeme (_, op@"+"), leftOp, rightOp] -> do
    logSnailAst "Op + Left" leftOp
    left <- fromSnail leftOp
    logSnailAst "Op + Right" rightOp
    right <- fromSnail rightOp
    pure $ Operation op [left, right]
  -- `(program X Y)` where `X` is some information, `Y` is an expression
  SExpression _ [Lexeme (_, "program"), info, body] -> do
    logSnailAst "Program info" info
    logSnailAst "Program body" body
    Program (Info info) <$> fromSnail body
  -- empty expressions are invalid
  SExpression _ [] -> throwError EmptyExpression
  -- expression of expressions, e.g. `((X))` -> `(X)`
  expr@(SExpression c exprs) -> do
    logSnailAst "Expression of expression" expr
    fromSnail . unwrap . SExpression c $ flatten exprs

requestInteger :: (MonadIO m) => m Integer
requestInteger = do
  liftIO $ putStrLn "Enter an integer"
  value <- liftIO getLine
  maybe requestInteger pure $ readMaybe @Integer value

data InterpreterError
  = InvalidOperation Ast
  | InvalidProgram Ast
  deriving stock (Show)

interpreter ::
  ( MonadLog (WithSeverity (Doc ann)) m
  , MonadIO m
  , MonadError InterpreterError m
  ) =>
  Ast ->
  m Integer
interpreter = \case
  Negate (AstInt int) -> pure int
  AstInt int -> pure int
  Read -> requestInteger
  Program _ program -> interpreter program
  ast@(Operation op operands) ->
    case (op, operands) of
      ("+", [x, y]) -> (+) <$> interpreter x <*> interpreter y
      ("-", [x]) -> negate <$> interpreter x
      _ -> throwError $ InvalidOperation ast
  ast -> throwError $ InvalidProgram ast

runM ::
  ( MonadIO m
  , MonadMask m
  ) =>
  ExceptT e (LoggingT (WithSeverity (Doc ann)) m) a ->
  m (Either e a)
runM program =
  withFDHandler defaultBatchingOptions stdout 0.4 80 $ \logToStdout ->
    flip runLoggingT (logToStdout . renderWithSeverity id) $
      runExceptT program

main :: IO ()
main = do
  eSExpressions <- readSnailFile "./programs/chapter1.snail"
  case eSExpressions of
    Left err -> print err
    Right [ast] -> do
      runM (fromSnail ast) >>= \case
        Left err -> putStrLn $ "Unable to read snail as L(int): " <> show err
        Right lInt -> do
          putStrLn $ Text.unpack (toText ast) <> " ==> " <> show lInt
          runM (interpreter lInt) >>= \case
            Right int -> putStrLn $ "Interpreter result: " <> show int
            Left err -> putStrLn $ "Interpreter error: " <> show err
    Right _ ->
      putStrLn "More than one S-expression is not allows in L(int)"
