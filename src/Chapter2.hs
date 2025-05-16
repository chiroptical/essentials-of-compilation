module Chapter2 where

import Data.Text (Text)

-- L_var
data LExpr
  = LInt Integer
  | LRead
  | LUnaryMinus LExpr
  | LPlus LExpr LExpr
  | LMinus LExpr LExpr
  | LVar Text
  | LLet Text LExpr LExpr

data LVar = LProgram () LExpr


-- C_var
data CVar = CProgram () CLabel CTail

newtype CLabel = CLabel Text

data CTail
  = CReturn CExpr
  | CSeq CStatement CTail

data CStatement = CAssign Text CExpr

data CAtom
  = CInt Integer
  | CVar Text

data CExpr
  = CAtomic CAtom
  | CRead
  | CUnaryMinus CAtom
  | CPlus CAtom CAtom
  | CMinus CAtom CAtom
