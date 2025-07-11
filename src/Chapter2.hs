module Chapter2 where

import Control.Monad.Except
import Control.Monad.Log
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter
import Snail
import Text.Read (readMaybe)

data LangError
  = TextLiteralUnsupported
  | EmptyExpression
  | InvalidLVarExpression
  deriving stock (Eq, Show)

-- L_var
data LExpr
  = LInt Integer
  | LRead
  | LUnaryMinus LExpr
  | LPlus LExpr LExpr
  | LMinus LExpr LExpr
  | LVar Text
  | LLet Text LExpr LExpr
  deriving stock (Eq, Show)

data LVar = LProgram () LExpr
  deriving stock (Eq, Show)

parseLeaf :: (MonadError LangError m) => Text -> m LExpr
parseLeaf = \case
  "read" -> pure LRead
  txt ->
    case readMaybe @Integer $ Text.unpack txt of
      Nothing -> pure $ LVar txt
      Just int -> pure $ LInt int

logSnailAst :: (MonadLog (WithSeverity (Doc ann)) m) => Text -> SnailAst -> m ()
logSnailAst msg expr = logInfo . pretty $ msg <> ": " <> toText expr

parseLExpr :: (MonadLog (WithSeverity (Doc ann)) m, MonadError LangError m) => SnailAst -> m LExpr
parseLExpr = \case
  -- `X` where `X` is a leaf in 'Ast'
  Lexeme (_, leaf) -> parseLeaf leaf
  -- no text literals are supported in this language
  TextLiteral _ -> throwError TextLiteralUnsupported
  -- `(- X)` where X is an integer or an S-expression
  SExpression _ _ [Lexeme (_, "-"), arg] -> do
    logSnailAst "Op -" arg
    operand <- parseLExpr arg
    pure $ LUnaryMinus operand
  -- `(- X)` where X is an integer or an S-expression
  SExpression _ _ [Lexeme (_, "-"), leftOp, rightOp] -> do
    logSnailAst "Op - Left" leftOp
    left <- parseLExpr leftOp
    logSnailAst "Op - Right" rightOp
    right <- parseLExpr rightOp
    pure $ LMinus left right
  -- `(+ X Y)` where X and Y are an integer or an S-expression
  SExpression _ _ [Lexeme (_, "+"), leftOp, rightOp] -> do
    logSnailAst "Op + Left" leftOp
    left <- parseLExpr leftOp
    logSnailAst "Op + Right" rightOp
    right <- parseLExpr rightOp
    pure $ LPlus left right
  SExpression _ _ [Lexeme (_, "let"), SExpression _ _ [Lexeme (_, name), binding], expr] -> do
    logSnailAst "Let binding" binding
    bin <- parseLExpr binding
    logSnailAst "Let expr" expr
    ex <- parseLExpr expr
    pure $ LLet name bin ex
  -- empty expressions are invalid
  SExpression _ _ [] -> throwError EmptyExpression
  -- expression of expressions, e.g. `((X))` -> `(X)`
  expr@(SExpression c b exprs) -> do
    logSnailAst "Expression of expression" expr
    parseLExpr . unwrap . SExpression c b $ unwrap <$> exprs

parseLVar :: (MonadLog (WithSeverity (Doc ann)) m, MonadError LangError m) => SnailAst -> m LVar
parseLVar = \case
  SExpression _ _ [Lexeme (_, "program"), info, body] -> do
    logSnailAst "Program info" info
    logSnailAst "Program body" body
    LProgram () <$> parseLExpr body
  ast -> do
    logSnailAst "Invalid LVar expression" ast
    throwError InvalidLVarExpression

-- C_var
data CAtom
  = CInt Integer
  | CVar Text

data CExpr
  = CAtomic CAtom
  | CRead
  | CUnaryMinus CAtom
  | CPlus CAtom CAtom
  | CMinus CAtom CAtom

data CStatement = CAssign Text CExpr

data CTail
  = CReturn CExpr
  | CSeq CStatement CTail

newtype CLabel = CLabel Text

data CVar = CProgram () CLabel CTail
