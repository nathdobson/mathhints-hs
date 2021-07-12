{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval where

import Ast
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Factors
import GHC.Base (assert)
import KMonad
import KReaderT
import Units
import Variants
import Value
import Number

type EvalCtx = Map String Value

data EvalState = EvalState{ inputTable :: Map }
newtype Eval t = Eval (KReaderT EvalCtx Compute t) deriving (KMonad, KMonadVariants)

withEvalCtx :: (EvalCtx -> EvalCtx) -> Eval t -> Eval t
withEvalCtx f (Eval x) = Eval $ kWithReader f x

eval :: (EvalCtx -> Compute t) -> Eval t
eval f = Eval $ KReaderT f

runEval :: Eval t -> EvalCtx -> Compute t
runEval (Eval x) = runKReader x

liftCompute :: Compute a -> Eval a
liftCompute = eval . const

evalPlus :: Value -> Value -> Value
evalPlus (Value d1 u1) (Value d2 u2) = assert (u1 == u2) (Value (d1 + d2) u1)

evalMinus :: Value -> Value -> Value
evalMinus (Value d1 u1) (Value d2 u2) = assert (u1 == u2) (Value (d1 - d2) u1)

evalSameUnit :: (Value -> Value -> Value) -> Value -> Value -> Compute Value
evalSameUnit f v1@(Value _ u1) v2@(Value _ u2) =
  if u1 == u2
    then kReturn $ f v1 v2
    else
      kUnion
        (kBind (convert u2 v1) (kReturn . f v2))
        (kBind (convert u1 v2) (kReturn . f v1))

evalTimes :: Value -> Value -> Compute Value
evalTimes (Value d1 u1) (Value d2 u2) = kReturn $ Value (d1 * d2) (factorsMul u1 u2)

evalDivide :: Value -> Value -> Compute Value
evalDivide (Value d1 u1) (Value d2 u2) = kReturn $ Value (d1 / d2) (factorsDiv u1 u2)

evalNegate :: Value -> Compute Value
evalNegate (Value d1 u1) = kReturn $ Value (- d1) u1

evalFun1 :: (Value -> Compute Value) -> Exp -> Eval Value
evalFun1 f e = kBind (evalExpr e) (liftCompute . f)

evalFun2 :: (Value -> Value -> Compute Value) -> Exp -> Exp -> Eval Value
evalFun2 f e1 e2 = kBind2 (evalExpr e1) (evalExpr e2) (\v1 v2 -> liftCompute $ f v1 v2)

evalUnitsIdentifier :: String -> Eval Factors
evalUnitsIdentifier n = kReturn $ Factors (Map.singleton n 1)

evalUnitsTerm :: UnitsTerm -> Eval Factors
evalUnitsTerm (UnitsIdent n) = evalUnitsIdentifier n
evalUnitsTerm (UnitsPow n p) = kBind (evalUnitsIdentifier n) (\n' -> kReturn $ factorsPow n' p)

evalUnitsTermList :: [UnitsTerm] -> Eval Factors
evalUnitsTermList xs = kBind (kSequence $ map evalUnitsTerm xs) (kReturn . foldl' factorsMul factorsEmpty)

evalUnitsExp :: UnitsExp -> Eval Factors
evalUnitsExp (UnitsProd x) = evalUnitsTermList x
evalUnitsExp (UnitsPer n d) = kBind2 (evalUnitsTermList n) (evalUnitsTermList d) (\n' d' -> kReturn $ factorsDiv n' d')

evalWithUnits :: Value -> Factors -> Eval Value
evalWithUnits (Value d u1) u2 =
  if u1 == factorsEmpty
    then kReturn (Value d u2)
    else error $ "Cannot apply units `" ++ show u2 ++ "` to value that already has units `" ++ show u1 ++ "`."

evalAsUnits :: Value -> Factors -> Eval Value
evalAsUnits v u = liftCompute (convert u v)

evalFun :: String -> [Value] -> Compute Value
evalFun = error "Unknown function"

evalExprOption :: ExpOption -> Eval Value
evalExprOption (Good g) = evalExpr g
evalExprOption (Bad b e) = kWithNote e $ evalExpr b

evalExpr :: Exp -> Eval Value
evalExpr e =
  case e of
    Plus e1 e2 -> evalFun2 (evalSameUnit evalPlus) e1 e2
    Minus e1 e2 -> evalFun2 (evalSameUnit evalMinus) e1 e2
    Times e1 e2 -> evalFun2 evalTimes e1 e2
    Divide e1 e2 -> evalFun2 evalDivide e1 e2
    Modulo e1 e2 -> undefined
    Negate e -> evalFun1 evalNegate e
    Less e1 e2 -> undefined
    LessEqual e1 e2 -> undefined
    Greater e1 e2 -> undefined
    GreaterEqual e1 e2 -> undefined
    Parens e -> evalExpr e
    Double n -> kReturn $ Value (fromConstant n) factorsEmpty
    Ident x -> eval (kReturn . fromJust . Map.lookup x)
    WithUnits e u -> kBind2 (evalExpr e) (evalUnitsExp u) evalWithUnits
    AsUnits e u -> kBind2 (evalExpr e) (evalUnitsExp u) evalAsUnits
    Call f es -> kSequence (map evalExpr es) `kBind` (liftCompute . evalFun f)
    Union es -> foldl' kUnion kEmpty (map evalExprOption es)

evalStmts :: [Stmt] -> Eval Value
evalStmts [] = error "Missing return"
evalStmts (Let x e : stmts) = evalExpr e `kBind` (\v -> withEvalCtx (Map.insert x v) (evalStmts stmts))
evalStmts [Return e] = evalExpr e
evalStmts (Return _ : _) = error "Early return"

runBlock :: [Stmt] -> Compute Value
runBlock block = runEval (evalStmts block) Map.empty
