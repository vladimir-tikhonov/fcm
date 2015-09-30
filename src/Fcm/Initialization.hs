module Fcm.Initialization (
  InitMethod(..),
  initMatrix
) where

import           Data.List.Split
import           Data.Matrix     as M
import           Fcm.Types
import           System.Random

data InitMethod = BelongingDegree | Centers deriving(Show, Read)

initMatrix :: InitMethod -> ObjectsMatrix -> RowsCount -> ClustersCount -> IO (BelongingMatrix)
initMatrix BelongingDegree _ = randU
-- initMatrix Centers =

randU :: RowsCount -> ClustersCount -> IO (BelongingMatrix)
randU nRows nClusters = do
  gen <- newStdGen
  let randList = take(nRows * nClusters) . randoms $ gen
      groupedList = chunksOf nClusters randList
      scaledList = map normalizeRow groupedList
      result = M.fromLists scaledList
  return (result)
  where normalizeRow row = map (/ (sum row)) row
