module Fcm.Types (
  BelongingMatrix,
  CentersMatrix,
  ObjectsMatrix,
  ClustersCount,
  RowsCount
) where

import           Data.Matrix as M

type BelongingMatrix = M.Matrix Double
type CentersMatrix = M.Matrix Double
type ObjectsMatrix = M.Matrix Double
type ClustersCount = Int
type RowsCount = Int
