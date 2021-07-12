module Factors (Factors (Factors, factors), factorsMul, factorsDiv, factorsPow, factorsEmpty, factorsFromListPair) where

import Data.List (foldl', intercalate, partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMaybeMatched)
import Data.Tuple.Extra (both, second)
import Unicode
import Util

newtype Factors = Factors {factors :: Map String Int} deriving (Eq, Ord)

factorsEmpty :: Factors
factorsEmpty = Factors Map.empty

factorsMul :: Factors -> Factors -> Factors
factorsMul (Factors a) (Factors b) = Factors $ addMapsInt a b

factorsReciprocal :: Factors -> Factors
factorsReciprocal (Factors a) = Factors $ Map.map (\x -> - x) a

factorsDiv :: Factors -> Factors -> Factors
factorsDiv a b = factorsMul a (factorsReciprocal b)

factorsPow :: Factors -> Int -> Factors
factorsPow f power = Factors $ Map.map (power *) $ factors f


factorsFromString :: String -> Factors
factorsFromString s = Factors $ Map.singleton s 1

factorsFromList :: [String] -> Factors
factorsFromList xs = foldl' factorsMul factorsEmpty $ map factorsFromString xs

factorsFromListPair :: [String] -> [String] -> Factors
factorsFromListPair n d =
  factorsDiv (factorsFromList n) (factorsFromList d)

instance Show Factors where
  show (Factors f) =
    let showSide xs = intercalate " " $ map (uncurry superscriptString) xs
        (num, denom) = partition ((> 0) . snd) $ Map.toList f
        denom' = map (second negate) denom
    in case (num, denom') of
          ([], []) -> ""
          ([], _ : _) -> showSide num
          (_ : _, []) -> "arc " ++ showSide denom'
          (_ : _, _ : _) -> showSide num ++ " per " ++ showSide denom'
