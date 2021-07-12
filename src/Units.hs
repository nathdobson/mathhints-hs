module Units where

import Data.List (foldl', foldl1')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMaybeMatched)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)
import Factors
import KMonad
import System.Etc.Internal.Types (filterMaybe)
import Variants
import Number

data Conversion = Ratio {ratio :: Double} deriving (Show)

data UnitData = UnitData {dimension :: Factors, conversion :: Conversion} deriving (Show)

data UnitCtx = UnitCtx {unitTable :: Map String UnitData} deriving (Show)

newtype Compute t = Compute {runCompute :: UnitCtx -> Variants t}

instance KMonad Compute where
  kBind x f = Compute (\ctx -> runCompute x ctx `kBind` (\y -> runCompute (f y) ctx))
  kReturn x = Compute (\_ -> kReturn x)

instance KMonadVariants Compute where
  kFromList x = Compute (\ctx -> kFromList x)
  kUnion (Compute f) (Compute g) = Compute (\ctx -> kUnion (f ctx) (g ctx))

withCanon :: [String] -> String -> UnitCtx -> UnitCtx
withCanon names dim (UnitCtx ctx) =
  UnitCtx $ foldl' (\ctx' name -> Map.insert name (UnitData (Factors (Map.singleton dim 1)) (Ratio 1.0)) ctx') ctx names

withRatio :: [String] -> Double -> [String] -> [String] -> UnitCtx -> UnitCtx
withRatio names r1 n d ctx@(UnitCtx m) =
  let UnitData dim (Ratio r2) = getUnitData ctx (factorsFromListPair n d)
      dat = UnitData dim $ Ratio $ r1 * r2 
   in UnitCtx $ foldl' (\m' name -> Map.insert name dat m') m names

conversionPow :: Conversion -> Int -> Conversion
conversionPow (Ratio x) power = Ratio (x ^^ power)

unitDataPow :: UnitData -> Int -> UnitData
unitDataPow (UnitData dim conv) power = UnitData (factorsPow dim power) (conversionPow conv power)

combineUnitData :: UnitData -> UnitData -> UnitData
combineUnitData (UnitData d1 (Ratio r1)) (UnitData d2 (Ratio r2)) = UnitData (factorsMul d1 d2) (Ratio (r1 * r2))

foldUnitData :: [UnitData] -> UnitData
foldUnitData [] = UnitData (Factors Map.empty) (Ratio 1.0)
foldUnitData xs = foldl1' combineUnitData xs

getOneConversion :: UnitCtx -> (String, Int) -> UnitData
getOneConversion ctx (name, power) = unitDataPow (fromMaybe (error $ "Unknown unit " ++ name) $ Map.lookup name $ unitTable ctx) power

getUnitData :: UnitCtx -> Factors -> UnitData
getUnitData ctx f = foldUnitData $ map (getOneConversion ctx) $ Map.toList $ factors f

toCanonical :: Conversion -> Number -> Number
toCanonical (Ratio x) y = fromConstant x * y

fromCanonical :: Conversion -> Number -> Number
fromCanonical (Ratio x) y = y / fromConstant x

