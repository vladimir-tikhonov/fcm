module Fcm.Types (
  FcmOpts(..),
  DistMethod(..),
  InitMethod(..),
  BelongingMatrix,
  CentersMatrix,
  ObjectsMatrix,
  ClustersCount,
  RowsCount
) where

import           Data.Matrix

data DistMethod = Hamming | Euclid deriving(Show, Read)
data InitMethod = BelongingDegree | Centers deriving(Show, Read)
data FcmOpts = FcmOpts { c :: ClustersCount, e :: Double, distMethod :: DistMethod, initMethod :: InitMethod }

type BelongingMatrix = Matrix Double
type CentersMatrix = Matrix Double
type ObjectsMatrix = Matrix Double
type ClustersCount = Int
type RowsCount = Int
