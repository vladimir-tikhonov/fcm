module Fcm.Fcm (
  DistMethod(..),
  InitMethod(..),
  FcmOpts(..),
  fcm
) where

import           Data.Matrix
import           Fcm.Calculations
import           Fcm.Distancies
import           Fcm.Initialization
import           Fcm.Types

fcm :: FcmOpts -> ObjectsMatrix -> IO BelongingMatrix
fcm opts x = do
  u <- initMatrix opts x (nrows x)
  let v = calcV u x
      newU = calcU v x opts
  return (calcURecursive u newU x opts)

calcURecursive :: BelongingMatrix -> BelongingMatrix -> ObjectsMatrix -> FcmOpts -> BelongingMatrix
calcURecursive oldM newM x opts
  | diffM oldM newM < e opts = newM
  | otherwise = calcURecursive newM (calcU newV x opts) x opts
    where newV = calcV newM x

diffM :: BelongingMatrix -> BelongingMatrix -> Double
diffM u1 u2 =
  maximum elemAbs
  where diffMatrix = elementwise (-) u1 u2
        elemList = toList diffMatrix
        elemAbs = map abs elemList
