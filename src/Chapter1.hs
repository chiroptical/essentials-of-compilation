module Chapter1 where

import Control.Monad (forM, forM_)
import Control.Monad.Trans.Except
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (trace)
import Snail.Shell
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

parseLexeme :: Text -> Except LangError Ast
parseLexeme = \case
  "read" -> pure Read
  txt ->
    case readMaybe @Integer $ Text.unpack txt of
      Nothing -> throwE $ UnknownLexeme txt
      Just int -> pure $ AstInt int

-- | TODO: Switch to `(MonadLogger m, MonadError m) => m Ast`? Allows me to more easily enable/disable logging.
fromSnail :: SExpression -> Except LangError Ast
fromSnail = \case
  -- no text literals
  TextLiteral _ -> throwE TextLiteralUnsupported
  SExpression _ ((TextLiteral _) : _ : _) -> throwE TextLiteralUnsupported
  -- `read` or `(read)`
  Lexeme (_, "read") -> pure Read
  SExpression _ [Lexeme (_, "read")] -> pure Read
  -- `X` could potentially be an 'AstInt'
  Lexeme (_, lexeme) -> parseLexeme lexeme
  -- `(- X)` where X is an integer or an S-expression
  SExpression _ [Lexeme (_, op@"-"), args] -> do
    operand <- fromSnail args
    pure . Negate $ Operation op [operand]
  -- `(+ X Y)` where X and Y are an integer or an S-expression
  SExpression c [Lexeme (_, op@"+"), leftOp, rightOp] -> do
    left <- fromSnail leftOp
    right <- fromSnail rightOp
    pure $ Operation op [left, right]
  -- `(program X Y)` where `X` is some information, `Y` is an expression
  SExpression _ [Lexeme (_, "program"), info, program] ->
    Program (Info info) <$> fromSnail program
  -- Empty expressions are invalid
  SExpression _ [] -> throwE EmptyExpression
  -- Any other lexemes are unknown
  expr@(SExpression _ [Lexeme (_, unknown), _]) ->
    throwE $ UnknownLexeme unknown
  -- expression of expressions
  SExpression c exprs ->
    fromSnail . SExpression c $ flatten exprs

main :: IO ()
main = do
  eSExpressions <- readSnailFile "./programs/chapter1.snail"
  case eSExpressions of
    Left err -> print err
    Right [snailAst] ->
      let lInt = runExcept $ fromSnail snailAst
       in putStrLn $ Text.unpack (toText snailAst) <> " ==> " <> show lInt
    Right _ ->
      putStrLn "More than one S-expression is not allows in L(int)"
