module Token where

data Token
  = TokenLet
  | TokenReturn
  | TokenAs
  | TokenIn
  | TokenPer
  | TokenGood
  | TokenBad
  | TokenSemicolon
  | TokenComma
  | TokenEqual
  | TokenEqualEqual
  | TokenLess
  | TokenLessEqual
  | TokenGreater
  | TokenGreaterEqual
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDivide
  | TokenModulo
  | TokenLeftParen
  | TokenRightParen
  | TokenLeftBrace
  | TokenRightBrace
  | TokenCaret
  | TokenIdent String
  | TokenString String
  | TokenInt Int
  | TokenDouble Double
  | TokenEof
  deriving (Eq, Show)
