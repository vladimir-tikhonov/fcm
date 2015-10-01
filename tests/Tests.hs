module Main where

import           Data.Matrix
import qualified Data.Vector        as V
import           Fcm.Calculations
import           Fcm.Distancies
import           Fcm.Initialization
import           Fcm.Types
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Distancies" $ do
    describe "Hamming" $ do
      it "returns correct distance" $ do
        let v1 = V.fromList [1, 2, 3]
            v2 = V.fromList [4, 5, 6]
        dist Hamming v1 v2 `shouldBe` (9 :: Double)
      it "correctly calculates zero distance" $ do
        let v1 = V.fromList [1, 2, 3]
            v2 = V.fromList [1, 2, 3]
        dist Hamming v1 v2 `shouldBe` (0 :: Double)

    describe "Euclid" $ do
      it "returns correct distance" $ do
        let v1 = V.fromList [1, 2]
            v2 = V.fromList [4, 6]
        dist Euclid v1 v2 `shouldBe` (5 :: Double)
      it "correctly calculates zero distance" $ do
        let v1 = V.fromList [1, 2, 3]
            v2 = V.fromList [1, 2, 3]
        dist Euclid v1 v2 `shouldBe` (0 :: Double)

  describe "Calculations" $
    describe "diffM" $ do
      it "returns maximum difference between elements in the same positions" $ do
        let m1 = fromLists [[1, 2, 3], [4, 5, 6]]
            m2 = fromLists [[2, 3, 4], [5, 6, 8]]
        diffM m1 m2 `shouldBe` (2 :: Double)

      it "returns 0 when matricies are equals" $ do
        let m1 = fromLists [[1, 2, 3], [4, 5, 6]]
            m2 = fromLists [[1, 2, 3], [4, 5, 6]]
        diffM m1 m2 `shouldBe` (0 :: Double)

  describe "Initialization" $
    describe "initMatrix" $ do
      describe "Centers" $ do
        let nClusters = 5
            nRows = 3
            opts = FcmOpts { c = nClusters, e = 0.0001, distMethod = Hamming, initMethod = Centers }
            x = fromLists [[1, 2], [3, 4], [5, 6]]
        it "returns matrix with nClusters columns" $ do
          u <- initMatrix opts x nRows
          ncols u `shouldBe` (nClusters :: Int)
        it "returns matrix with each row's sum eq 1" $ do
          u <- initMatrix opts x nRows
          let list = toLists u
              sums = map (round . sum) list
          sums `shouldBe` replicate nRows (1 :: Int)

      describe "BelongingDegree" $ do
        let nClusters = 5
            nRows = 3
            opts = FcmOpts { c = nClusters, e = 0.0001, distMethod = Hamming, initMethod = BelongingDegree }
            x = fromLists [[1, 2], [3, 4], [5, 6]]
        it "returns matrix with nClusters columns" $ do
          u <- initMatrix opts x nRows
          ncols u `shouldBe` (nClusters :: Int)
        it "returns matrix with each row's sum eq 1" $ do
          u <- initMatrix opts x nRows
          let list = toLists u
              sums = map (round . sum) list
          sums `shouldBe` replicate nRows (1 :: Int)
