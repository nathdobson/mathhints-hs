module Lib
  ( someFunc,
    runScript,
  )
where

import Defaults
import Eval
import Lexer
import Parser
import Units
import Variants
import Value

someFunc :: IO ()
someFunc = putStrLn "someFunc"

runScript :: String -> Either String (Variants Value)
runScript code = do
  tokens <- lexer code
  stmts <- parser tokens
  return $ runCompute (runBlock stmts) defaultCtx
