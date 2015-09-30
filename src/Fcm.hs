module Fcm (
  DistMethod(..),
  InitMethod(..),
  FcmOpts(..),
  fcmTest
) where

import           Data.List.Split
import           Data.Matrix     as M
import qualified Data.Vector     as V
import           System.Random

fcmTest :: FcmOpts -> ObjectsMatrix -> IO (BelongingMatrix)
fcmTest opt o = do
  m <- initMatrix BelongingDegree (nrows o) (c opt)
  let centers = calcCenters m o
  return (centers)

type BelongingMatrix = M.Matrix Double
type CentersMatrix = M.Matrix Double
type ObjectsMatrix = M.Matrix Double
type ClustersCount = Int
type RowsCount = Int

data DistMethod = Hamming | Euclid deriving(Show, Read)

dist :: DistMethod -> V.Vector Double -> V.Vector Double -> Double
dist Hamming v1 v2 =
  V.foldl' (+) 0 zippedAbs
  where zipped = V.zipWith (-) v1 v2
        zippedAbs = V.map abs zipped

dist Euclid v1 v2 =
  sqrt $ V.foldl' (+) 0 squares
  where zipped = V.zipWith (-) v1 v2
        squares = V.map (** 2) zipped

data InitMethod = BelongingDegree | Centers deriving(Show, Read)

initMatrix :: InitMethod -> RowsCount -> ClustersCount -> IO (BelongingMatrix)
initMatrix BelongingDegree = randIdentityMatrix

calcCenters :: BelongingMatrix -> ObjectsMatrix -> CentersMatrix
calcCenters m x =
  foldr1 (<->) centerMatricies
  where n = nrows m
        xRow i = rowVector $ getRow i x
        matrixSum = elementwise (+)
        scaledElem i l = (getElem i l m) ** 2 -- (μ_il)^m, m = 2
        scaledObject i l = scaleMatrix (scaledElem i l) (xRow i) -- (μ_il)^m * X_i
        scaledObjects l = map (\i -> scaledObject i l) [1..n] -- [ (μ_il)^m * X_i ], i = 1..n
        scaledObjectsSum l = foldr1 matrixSum (scaledObjects l) -- Sum [ .. ]
        centerScaleCoef l = 1 / (V.sum $ getCol l m) -- Sum (μ_il)
        centerMatricies = map (\l -> scaleMatrix (centerScaleCoef l) (scaledObjectsSum l) ) [1..ncols m]

randIdentityMatrix :: RowsCount -> ClustersCount -> IO (BelongingMatrix)
randIdentityMatrix nRows nClusters = do
  gen <- newStdGen
  let randList = take(nRows * nClusters) . randoms $ gen
      groupedList = chunksOf nClusters randList
      scaledList = map normalizeRow groupedList
      result = M.fromLists scaledList
  return (result)
  where normalizeRow row = map (/ (sum row)) row

data FcmOpts = FcmOpts { c :: ClustersCount, e :: Double, distMethod :: DistMethod, initMethod :: InitMethod }
