module Number (Number, fromConstant, fromVariable, toDouble) where

import Data.Map (Map)
import qualified Data.Map as Map
import Util

data Number = Number {toDouble :: Double, derivatives :: Map Int Double} deriving (Eq, Ord, Show)

fromConstant :: Double -> Number
fromConstant x = Number x Map.empty

fromVariable :: Double -> Int -> Number
fromVariable x n = Number x (Map.singleton n 1)

nmap :: (Double -> (Double, Double)) -> Number -> Number
nmap f (Number v d) =
  let (fv, fd) = f v
   in Number fv (Map.map (fd *) d)

nmap2 :: (Double -> Double -> (Double, Double, Double)) -> Number -> Number -> Number
nmap2 f (Number v1 d1) (Number v2 d2) =
  let (fv, fd1, fd2) = f v1 v2
   in Number fv (addMapsDouble (Map.map (fd1 *) d1) (Map.map (fd2 *) d2))

instance Num Number where
  (+) = nmap2 $ \x y -> (x + y, 1, 1)
  (-) = nmap2 $ \x y -> (x - y, 1, -1)
  (*) = nmap2 $ \x y -> (x * y, y, x)
  negate = nmap $ \x -> (- x, -1)
  signum = nmap $ \x -> (signum x, 0)
  fromInteger = fromConstant . fromInteger
  abs = nmap $ \x -> if x < 0 then (- x, -1) else (x, 1)

instance Fractional Number where
  (/) = nmap2 $ \x y -> (x / y, 1 / y, - x / (y * y))
  recip = nmap $ \x -> (1 / x, -1 / (x * x))
  fromRational = fromConstant . fromRational
