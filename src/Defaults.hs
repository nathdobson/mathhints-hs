{-# LANGUAGE TupleSections #-}

module Defaults where

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Factors
import Units

defaultCtx :: UnitCtx
defaultCtx =
  foldl'
    (\c f -> f c)
    (UnitCtx Map.empty)
    [ --
      withCanon ["m", "meter", "meters"] "length",
      withRatio ["ft", "foot", "feet"] 0.3048 ["meter"] [],
      withRatio ["mi", "mile", "miles"] 1609.344 ["meter"] [],
      --
      withCanon ["s", "second", "seconds"] "time",
      withRatio ["min", "minute", "minutes"] 60.0 ["seconds"] [],
      withRatio ["h", "hour", "hours"] 60.0 ["minutes"] [],
      withRatio ["day", "days"] 24.0 ["hours"] [],
      --
      id
    ]

--  UnitCtx $
--    Map.fromList $ do
--      (names, num, denom, ratio) <-
--        [ (["m", "meter", "meters"], ["length"], [], 1.0),
--          (["ft", "foot", "feet"], ["length"], [], 0.3048),
--          (["s", "second", "seconds"], ["time"], [], 1.0),
--          (["min", "minute", "minutes"], ["time"], [], 60.0),
--          (["h", "hour", "hours"], ["time"], [], 3600.0),
--          (["day", "days"], ["time"], [], 3600.0 * 24.0)
--          ]
--      name <- names
--      return
--        ( name,
--          UnitData
--            ( factorsDiv
--                (Factors (Map.fromListWith (+) $ map (,1) num))
--                (Factors (Map.fromListWith (+) $ map (,1) denom))
--            )
--            (Ratio ratio)
--        )
