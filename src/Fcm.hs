module Fcm (
  DistMethod(..),
  InitMethod(..)
) where

import qualified Data.Vector as V

data DistMethod = Hamming | Euclid deriving(Show, Read)

dist :: DistMethod -> V.Vector Double -> V.Vector Double -> Double
dist Hamming v1 v2 =
  V.foldl' (+) 0 zippedAbs
  where zipped = V.zipWith (-) v1 v2
        zippedAbs = V.map abs zipped

dist Euclid v1 v2 =
  sqrt $ V.foldl' (+) 0 squares
  where zipped = V.zipWith (-) v1 v2
        squares = V.map (^ (2 :: Int)) zipped

data InitMethod = BelongingDegree | Centers deriving(Show, Read)

data FcmOpts = FcmOpts { c :: Int, e :: Double, distMethod :: DistMethod, initMethod :: InitMethod }
