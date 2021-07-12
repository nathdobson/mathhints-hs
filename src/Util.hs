module Util where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict (merge)
import Data.Map.Merge.Strict (preserveMissing)
import Data.Map.Merge.Strict (zipWithMaybeMatched)

addMapsInt :: Ord a => Map a Int -> Map a Int -> Map a Int
addMapsInt = unionWithKeyMaybe (\_ v1 v2 -> let v = v1 + v2 in if v == 0 then Nothing else Just v)

addMapsDouble :: Ord a => Map a Double -> Map a Double -> Map a Double
addMapsDouble = unionWithKeyMaybe (\_ v1 v2 -> Just $ v1 + v2)

unionWithKeyMaybe :: Ord k => (k -> v -> v -> Maybe v) -> Map k v -> Map k v -> Map k v
unionWithKeyMaybe f = merge preserveMissing preserveMissing (zipWithMaybeMatched f)
