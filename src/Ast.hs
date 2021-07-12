module Ast where

data UnitsTerm
  = UnitsIdent String
  | UnitsPow String Int
  deriving (Eq, Show)

data UnitsExp
  = UnitsProd [UnitsTerm]
  | UnitsPer [UnitsTerm] [UnitsTerm]
  deriving (Eq, Show)

data ExpOption
  = Good Exp
  | Bad Exp String
  deriving (Eq, Show)

data Exp
  = Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Divide Exp Exp
  | Modulo Exp Exp
  | Negate Exp
  | Less Exp Exp
  | LessEqual Exp Exp
  | Greater Exp Exp
  | GreaterEqual Exp Exp
  | Call String [Exp]
  | WithUnits Exp UnitsExp
  | AsUnits Exp UnitsExp
  | Union [ExpOption]
  | Parens Exp
  | Double Double
  | Ident String
  deriving (Eq, Show)

data Stmt
  = Let String Exp
  | Return Exp
  deriving (Eq, Show)

data Block = Block [Stmt]
