{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Variants (KMonadVariants, Variants, runVariants, kFromList, toList, kUnion, kEmpty, kBad, kBads, kGoods, kWithNote) where

import Control.Arrow (second)
import Data.List (foldl', intercalate, transpose)
import Data.List.Extra (groupSort, mergeBy)
import Data.List.Index (indexed)
import Data.List.Key (merge)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra ((&&&))
import Data.Universe.Helpers (cartesianProduct)
import KMonad

class KMonad m => KMonadVariants m where
  kFromList :: Ord t => [(t, [String])] -> m t
  kUnion :: Ord a => m a -> m a -> m a
  kEmpty :: Ord t => m t
  kEmpty = kFromList []
  kBad :: Ord t => t -> String -> m t
  kBad x n = kFromList [(x, [n])]
  kBads :: Ord t => t -> [String] -> m t
  kBads x ns = kFromList [(x, ns)]
  kGoods :: Ord t => [t] -> m t
  kGoods xs = kFromList $ map (,[]) xs
  kWithNote :: Ord t => String -> m t -> m t
  kWithNote n xs = xs `kBind` (`kBad` n)

newtype Variants t = Variants {runVariants :: [Map t (Set [String])]} deriving (Eq)

instance KMonad Variants where
  kBind = vbind
  kReturn x = kFromList [(x, [])]

instance KMonadVariants Variants where
  kFromList pairs = Variants $ map mapFromList $ mergeByIndex (length . snd) pairs
  kUnion (Variants l1) (Variants l2) =
    Variants $ zipWithMerge (Map.unionWith Set.union) l1 l2

mapFromList :: Ord k => Ord v => [(k, v)] -> Map k (Set v)
mapFromList xs = Map.fromListWith Set.union $ map (second Set.singleton) xs

mergeKeyValueList :: Ord k => (v -> v -> v) -> [(k, v)] -> [(k, v)] -> [(k, v)]
mergeKeyValueList _ [] [] = []
mergeKeyValueList _ [] _ = []
mergeKeyValueList _ xs1 [] = xs1
mergeKeyValueList f xs1a@((k1, v1) : xs1b) xs2a@((k2, v2) : xs2b) =
  case compare k1 k2 of
    LT -> (k1, v1) : mergeKeyValueList f xs1b xs2a
    EQ -> (k1, f v1 v2) : mergeKeyValueList f xs1b xs2b
    GT -> (k2, v2) : mergeKeyValueList f xs1a xs2b

listFromIndexPairs :: a -> [(Int, a)] -> [a]
listFromIndexPairs def xs = map snd $ mergeKeyValueList const xs (indexed $ repeat def)

mergeByIndex :: (a -> Int) -> [a] -> [[a]]
mergeByIndex index xs = listFromIndexPairs [] $ groupSort $ map (index &&& id) xs

toList :: Ord t => Variants t -> [(t, [String])]
toList (Variants xs) = do
  for_count <- xs
  (value, notes_set) <- Map.toList for_count
  notes <- Set.toList notes_set
  return (value, notes)

zipWithMerge :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithMerge _ xs [] = xs
zipWithMerge _ [] ys = ys
zipWithMerge f (x : xs) (y : ys) = f x y : zipWithMerge f xs ys

prepend :: Set [String] -> [Map a (Set [String])] -> [Map a (Set [String])]
prepend ns1 xs =
  let ns1' = Set.toList ns1
   in map (Map.map (Set.fromList . cartesianProduct (++) ns1' . Set.toList)) xs

unifyList :: Ord a => [Map a (Set [String])] -> Map a (Set [String])
unifyList = foldl' (Map.unionWith Set.union) Map.empty

vbindRec :: Ord a => Ord b => [Map a (Set [String])] -> (a -> Variants b) -> [Map b (Set [String])]
vbindRec [] _ = []
vbindRec (xs : xss) f =
  let with_xs = map (\(x, nss) -> prepend nss (runVariants $ f x)) $ Map.toList xs
      with_xss = Map.empty : vbindRec xss f
   in map unifyList $ transpose (with_xs ++ [with_xss])

vbind :: Ord a => Ord b => Variants a -> (a -> Variants b) -> Variants b
vbind (Variants xs) f = Variants $ vbindRec xs f

showOption :: Show a => (a, [String]) -> String
showOption (x, []) = "good " ++ show x
showOption (x, ns) = "bad " ++ show x ++ " " ++ show ns

instance (Ord x, Show x) => Show (Variants x) where
  show m = "{" ++ intercalate ", " (map showOption (toList m)) ++ "}"
