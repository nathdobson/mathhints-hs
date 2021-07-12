import Ast
import Data.Either.Unwrap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void
import Debug.Trace (traceShow)
import Defaults
import Factors (Factors (Factors))
import KMonad
import Lexer
import Lib
import Parser
import Test.HUnit
import Token
import Units
import Variants
import Value

testVariants :: Test
testVariants = TestCase $ do
  assertEqual "" [] (runVariants (kEmpty :: Variants Void))
  assertEqual "" [] (toList (kEmpty :: Variants Void))
  assertEqual "" [Map.singleton 1 (Set.singleton [])] (runVariants $ kReturn 1)
  assertEqual "" [(1, [])] (toList $ kReturn 1)
  assertEqual "" [Map.empty, Map.singleton 2 (Set.singleton ["kBad"])] (runVariants $ kBad 2 "kBad")
  assertEqual "" [(2, ["kBad"])] (toList $ kBad 2 "kBad")
  assertEqual "" [Map.fromList [(3, Set.singleton []), (4, Set.singleton [])]] (runVariants $ kGoods [3, 4])
  assertEqual "" [(3, []), (4, [])] (toList $ kGoods [3, 4])
  assertEqual "" (kReturn 3 :: Variants Int) (kReturn 1 `kBind` (\x -> kReturn (x + 2)))
  assertEqual "" (kBad 3 "kBad" :: Variants Int) (kBad 1 "kBad" `kBind` (\x -> kReturn (x + 2)))
  assertEqual "" (kBad 3 "kBad" :: Variants Int) (kReturn 1 `kBind` (\x -> kBad (x + 2) "kBad"))
  assertEqual "" (kBads 3 ["x", "y"] :: Variants Int) (kBad 1 "x" `kBind` (\x -> kBad (x + 2) "y"))
  assertEqual "" (kGoods [2, 4] :: Variants Int) (kGoods [1, 3] `kBind` (\x -> kReturn (x + 1)))
  assertEqual "" (kGoods [2, 4] :: Variants Int) (kReturn 1 `kBind` (\x -> kGoods [x + 1, x + 3]))
  assertEqual "" (kGoods [1, 2, 3, 4] :: Variants Int) (kGoods [0, 2] `kBind` (\x -> kGoods [x + 1, x + 2]))
  assertEqual "" (kGoods [1, 2, 3, 4] :: Variants Int) (kGoods [0, 2] `kBind` (\x -> kGoods [1, 2] `kBind` (\y -> kReturn (x + y))))

fromOk :: Show a => Either a b -> b
fromOk (Left e) = error (show e)
fromOk (Right v) = v

testLexer :: Test
testLexer = TestCase $ do
  assertEqual "a" (lexer "") (Right [])
  assertEqual "b" (lexer "let") (Right [TokenLet])
  assertEqual "c" (lexer "x") (Right [TokenIdent "x"])
  assertEqual "d" (lexer " ") (Right [])
  assertEqual
    "e"
    (lexer "let x = 1; return x;")
    ( Right
        [ TokenLet,
          TokenIdent "x",
          TokenEqual,
          TokenInt 1,
          TokenSemicolon,
          TokenReturn,
          TokenIdent "x",
          TokenSemicolon
        ]
    )
  return ()

testParser :: Test
testParser = TestCase $ do
  assertEqual "" (parser =<< lexer "let x = 1; return x;") (Right [Let "x" (Double 1.0), Return (Ident "x")])
  assertEqual "f" (parser =<< lexer "return foo(bar);") (Right [Return (Call "foo" [Ident "bar"])])
  assertEqual "f" (parser =<< lexer "return foo(bar,baz);") (Right [Return (Call "foo" [Ident "bar", Ident "baz"])])
  return ()

testEval :: Test
testEval = TestCase $ do
  assertEqual
    ""
    (runScript "return 1.0;")
    (Right $ kReturn $ unitless 1.0)
  assertEqual
    ""
    (runScript "let x = 2.0; let y = 3.0; return x + x * y;")
    (Right $ kReturn $ unitless 8.0)
  assertEqual
    ""
    ( runScript
        " let x = 2 minutes; \
        \ let y = 60 meters per hour; \
        \ return x * y in feet;"
    )
    (Right $ kReturn $ Value 6.561679790026246 $ Factors $ Map.singleton "feet" 1)
  assertEqual
    ""
    ( runScript
        " let x = 15 miles per hour; \
        \ let y = 30 miles per hour; \
        \ return { good 2/( 1/x + 1/y ), bad (x+y)/2 \"geometric average\" } in miles per hour;"
    )
    ( Right $
        kUnion
          (kReturn $ Value 20.0 $ Factors $ Map.fromList [("miles", 1), ("hour", -1)])
          (kBad (Value 22.500000000000004 $ Factors $ Map.fromList [("miles", 1), ("hour", -1)]) "geometric average")
    )
  return ()

tests :: Test
tests =
  TestList
    [ TestLabel "testVariants" testVariants,
      TestLabel "testLexer" testLexer,
      TestLabel "testParser" testParser,
      TestLabel "testEval" testEval
    ]

main :: IO Counts
main = runTestTT tests
