module Fcm.Fcm (
  DistMethod(..),
  InitMethod(..),
  FcmOpts(..),
  fcm
) where

import           Data.Matrix        as M
import qualified Data.Vector        as V
import           Fcm.Distancies
import           Fcm.Initialization
import           Fcm.Types

data FcmOpts = FcmOpts { c :: ClustersCount, e :: Double, distMethod :: DistMethod, initMethod :: InitMethod }

fcm :: FcmOpts -> ObjectsMatrix -> IO (BelongingMatrix)
fcm opts x = do
  u <- initMatrix (initMethod opts) x (nrows x) (c opts)
  let v = calcV u x
      newU = calcU v x opts
  return (calcURecursive u newU x opts)

calcURecursive :: BelongingMatrix -> BelongingMatrix -> ObjectsMatrix -> FcmOpts -> BelongingMatrix
calcURecursive oldM newM x opts
  | diffM oldM newM < e opts = newM
  | otherwise = calcURecursive newM (calcU newV x opts) x opts
    where newV = calcV newM x

calcV :: BelongingMatrix -> ObjectsMatrix -> CentersMatrix
calcV m x =
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

calcU :: CentersMatrix -> ObjectsMatrix -> FcmOpts -> BelongingMatrix
calcU v x opts =
  fromLists result
  where xRow i = getRow i x
        vRow i = getRow i v
        calcDist v1 v2 = dist (distMethod opts) v1 v2
        calcElem i j k = (calcDist (xRow i) (vRow k)) / (calcDist (xRow i) (vRow j)) -- d(X_i, V_k) / d(X_i, V_j)
        elemPow i k j = (calcElem i j k) ** 2 -- (d(...) / d(...)) ^ 2/(m - 1)
        elemPows i k = map (elemPow i k) [1..nrows v] -- [...], i = 1..c
        elemSum i k = foldr1 (+) (elemPows i k) -- Sum [...]
        elemSumInv i k = 1 / (elemSum i k) -- (Sum [...]) ^ -1
        elemRow i = map (elemSumInv i) [1..nrows v]
        result = map elemRow [1..nrows x]

diffM :: BelongingMatrix -> BelongingMatrix -> Double
diffM u1 u2 =
  maximum elemAbs
  where diffMatrix = elementwise (-) u1 u2
        elemList = toList diffMatrix
        elemAbs = map abs elemList
