module Fcm.Distancies (
  DistMethod(..),
  dist
) where

import qualified Data.Vector as V
import           Fcm.Types

dist :: DistMethod -> V.Vector Double -> V.Vector Double -> Double
dist Hamming v1 v2 =
  V.sum zippedAbs
  where zipped = V.zipWith (-) v1 v2
        zippedAbs = V.map abs zipped

dist Euclid v1 v2 =
  sqrt $ V.sum squares
  where zipped = V.zipWith (-) v1 v2
        squares = V.map (** 2) zipped
