module Fcm (
  DistMethod(..),
  InitMethod(..)
) where

import qualified Data.Vector as V

data DistMethod = Hamming | Euclid deriving(Show, Read)

data InitMethod = BelongingDegree | Centers deriving(Show, Read)

data FcmOpts = FcmOpts { c :: Int, e :: Double, distMethod :: DistMethod, initMethod :: InitMethod }
