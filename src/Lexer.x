{
module Lexer where

import Token
import Text.Read (readMaybe)

}

%wrapper "monad"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-
  $white+                                  ;
  let                                  { mk $ \_ -> TokenLet }
  as                                   { mk $ \_ -> TokenAs }
  in                                   { mk $ \_ -> TokenIn }
  per                                  { mk $ \_ -> TokenPer }
  return                               { mk $ \_ -> TokenReturn }
  good                                 { mk $ \_ -> TokenGood }
  bad                                  { mk $ \_ -> TokenBad }
  [$digit]+\.[$digit]+                 { mkDouble }
  [$digit]+                            { mkInt }
  \;                                   { mk $ \_ -> TokenSemicolon }
  \,                                   { mk $ \_ -> TokenComma }
  \=                                   { mk $ \_ -> TokenEqual }
  \=\=                                 { mk $ \_ -> TokenEqualEqual }
  \<                                   { mk $ \_ -> TokenLess }
  \<\=                                 { mk $ \_ -> TokenLessEqual }
  \>                                   { mk $ \_ -> TokenGreater }
  \>\=                                 { mk $ \_ -> TokenGreaterEqual }
  \+                                   { mk $ \_ -> TokenPlus }
  \-                                   { mk $ \_ -> TokenMinus }
  \*                                   { mk $ \_ -> TokenTimes }
  \/                                   { mk $ \_ -> TokenDivide }
  \%                                   { mk $ \_ -> TokenModulo }
  \^                                   { mk $ \_ -> TokenCaret }
  \(                                   { mk $ \_ -> TokenLeftParen  }
  \)                                   { mk $ \_ -> TokenRightParen }
  \{                                   { mk $ \_ -> TokenLeftBrace }
  \}                                   { mk $ \_ -> TokenRightBrace }
  \"(\\.|[^\"\\])*\"                   { mkString }
  $alpha [$alpha $digit \_]*           { mk $ \s -> TokenIdent s }

{
mk :: (String -> Token) -> AlexInput -> Int -> Alex Token
mk f (_,_,_,str) len = return (f (take len str))

mkEither :: (String -> Either String Token) -> AlexInput -> Int -> Alex Token
mkEither f (_,_,_,str) len = either alexError return $ f (take len str)

mkDouble :: AlexInput -> Int -> Alex Token
mkDouble = mkEither $ \s -> maybe (Left $ "Error parsing double `" ++ s ++ "`") (Right . TokenDouble) $ readMaybe s

mkInt :: AlexInput -> Int -> Alex Token
mkInt = mkEither $ \s -> maybe (Left $ "Error parsing int `" ++ s ++ "`") (Right . TokenInt) $ readMaybe s

readStringSuffix :: String -> Either String String
readStringSuffix "\"" = Right ""
readStringSuffix [c] = Left "Missing end quote"
readStringSuffix ('\\' : c : xs) = (c:) <$> readStringSuffix xs
readStringSuffix (c : xs) = (c:) <$> readStringSuffix xs

readStringLiteral :: String -> Either String String
readStringLiteral ('"' : xs) = readStringSuffix xs
readStringLiteral [] = Left "Missing starting quote"

mkString :: AlexInput -> Int -> Alex Token
mkString = mkEither (\x -> TokenString <$> readStringLiteral x)

alexEOF :: Alex Token
alexEOF = return TokenEof

alexLex :: Alex [Token]
alexLex = do token <- alexMonadScan
             if token == TokenEof
               then return []
               else (token:) <$> alexLex

lexer :: String -> Either String [Token]
lexer str = runAlex str alexLex

}