module Value where

import Number
import Factors
import Units
import KMonad

data Value = Value {value :: Number, valueUnits :: Factors} deriving (Eq, Ord)

convert :: Factors -> Value -> Compute Value
convert to (Value real from) = Compute $ \ctx ->
  let to_data = getUnitData ctx to
      from_data = getUnitData ctx from
   in if dimension to_data == dimension from_data
        then kReturn $ Value (fromCanonical (conversion to_data) $ toCanonical (conversion from_data) real) to
        else error $ "Cannot convert from " ++ show (dimension from_data) ++ " to " ++ show (dimension to_data)

unitless :: Number -> Value
unitless x = Value x factorsEmpty

instance Show Value where
  show (Value d u) = show d ++ " " ++ show u