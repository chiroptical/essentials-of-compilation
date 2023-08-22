{-# LANGUAGE FlexibleContexts #-}

module Chapter1 where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Log
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter
import Snail.Shell
import System.IO (stdout)
import Text.Read (readMaybe)

-- | TODO: Unsure what this is used for yet
newtype Info = Info SExpression
  deriving stock (Show)

data LangError
  = TextLiteralUnsupported
  | EmptyExpression
  | UnknownLexeme Text
  deriving stock (Show)

-- | The AST for $\mathcal{L}_\mathrm{int}$
data Ast
  = Program Info Ast
  | AstInt Integer
  | Operation Text [Ast]
  | Negate Ast
  | Read
  deriving stock (Show)

flatten :: [SExpression] -> [SExpression]
flatten = \case
  [] -> []
  x@[Lexeme {}] -> x
  x@[TextLiteral {}] -> x
  [SExpression _ x] -> x
  (SExpression _ x : rest) -> flatten x <> flatten rest
  other -> error $ show other

parseLexeme :: (MonadError LangError m) => Text -> m Ast
parseLexeme = \case
  "read" -> pure Read
  txt ->
    case readMaybe @Integer $ Text.unpack txt of
      Nothing -> throwError $ UnknownLexeme txt
      Just int -> pure $ AstInt int

logSExpression :: (MonadLog (WithSeverity (Doc ann)) m) => Text -> SExpression -> m ()
logSExpression msg expr = logInfo . pretty $ msg <> ": " <> toText expr

-- | TODO: Switch to `(MonadLogger m, MonadError m) => m Ast`? Allows me to more easily enable/disable logging.
fromSnail :: (MonadLog (WithSeverity (Doc ann)) m, MonadError LangError m) => SExpression -> m Ast
fromSnail = \case
  -- no text literals
  TextLiteral _ -> throwError TextLiteralUnsupported
  SExpression _ ((TextLiteral _) : _ : _) -> throwError TextLiteralUnsupported
  -- `X` could potentially be an 'AstInt' or 'Read'
  Lexeme (_, lexeme) -> parseLexeme lexeme
  -- `(read)`
  SExpression _ [Lexeme (_, "read")] -> pure Read
  -- `(- X)` where X is an integer or an S-expression
  SExpression _ [Lexeme (_, op@"-"), arg] -> do
    logSExpression "Op -" arg
    operand <- fromSnail arg
    pure . Negate $ Operation op [operand]
  -- `(+ X Y)` where X and Y are an integer or an S-expression
  SExpression c [Lexeme (_, op@"+"), leftOp, rightOp] -> do
    logSExpression "Op + Left" leftOp
    left <- fromSnail leftOp
    logSExpression "Op + Right" rightOp
    right <- fromSnail rightOp
    pure $ Operation op [left, right]
  -- `(program X Y)` where `X` is some information, `Y` is an expression
  SExpression _ [Lexeme (_, "program"), info, body] -> do
    logSExpression "Program info" info
    logSExpression "Program body" body
    Program (Info info) <$> fromSnail body
  -- Empty expressions are invalid
  SExpression _ [] -> throwError EmptyExpression
  -- Any other lexemes are unknown
  expr@(SExpression _ [Lexeme (_, unknown), _]) -> do
    logSExpression "Unknown expression of lexeme" expr
    throwError $ UnknownLexeme unknown
  -- expression of expressions
  expr@(SExpression c exprs) -> do
    logSExpression "Expression of expressions: " expr
    fromSnail . SExpression c $ flatten exprs

main :: IO ()
main = do
  eSExpressions <- readSnailFile "./programs/chapter1.snail"
  case eSExpressions of
    Left err -> print err
    Right [snailAst] -> do
      lInt <-
        withFDHandler defaultBatchingOptions stdout 0.4 80 $ \logToStdout ->
          flip runLoggingT (logToStdout . renderWithSeverity id) $ runExceptT $ fromSnail snailAst
      putStrLn $ Text.unpack (toText snailAst) <> " ==> " <> show lInt
    Right _ ->
      putStrLn "More than one S-expression is not allows in L(int)"
