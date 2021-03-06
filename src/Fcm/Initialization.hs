module Fcm.Initialization (
  InitMethod(..),
  initMatrix
) where

import           Data.List.Split
import           Data.Matrix
import qualified Data.Random        as R
import           Data.Random.Extras
import           Data.Random.RVar
import           Fcm.Calculations
import           Fcm.Types
import           System.Random

initMatrix :: FcmOpts -> ObjectsMatrix -> RowsCount -> IO BelongingMatrix
initMatrix FcmOpts { initMethod = BelongingDegree, c = nClusters } _ r = randU r nClusters

initMatrix opts@FcmOpts { initMethod = Centers, c = nClusters } x _ = do
  v <- randV nClusters x
  let vExt = extendV nClusters v
  return (calcU vExt x opts)

randV :: ClustersCount -> ObjectsMatrix -> IO CentersMatrix
randV nClusters x = do
  randX <- runRVar rvar R.StdRandom
  return (fromLists randX)
  where rvar = sample nClusters . toLists $ x

extendV :: ClustersCount -> CentersMatrix -> CentersMatrix
extendV nClusters v
  | nrows v >= nClusters = v
  | otherwise = v <-> fromLists (replicate (nClusters - nrows v) (replicate (ncols v) 0.0))

randU :: ClustersCount -> ClustersCount -> IO BelongingMatrix
randU nRows nClusters = do
  gen <- newStdGen
  let randList = take(nRows * nClusters) . randoms $ gen
      groupedList = chunksOf nClusters randList
      scaledList = map normalizeRow groupedList
      result = fromLists scaledList
  return result
  where normalizeRow row = map (/ sum row) row
