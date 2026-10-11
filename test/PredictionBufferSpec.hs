module PredictionBufferSpec (spec) where

import Codec.Picture.WebP.Internal.VP8.Predict
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word8)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Compact VP8 prediction buffers" $ do
    it "matches all frame predictors at corners, edges, and interior blocks" $ do
      forM_ [8, 16] $ \size ->
        forM_ [0 .. 3] $ \mode ->
          forM_ [(x, y) | x <- [0, size, size * 2], y <- [0, size, size * 2]] $ \(x, y) ->
            forM_ [0, 1, 2] $ \pixelPattern -> do
              let stride = size * 3
                  pixels = VS.generate (stride * stride) $ \i -> case pixelPattern :: Int of
                    0 -> fromIntegral ((i * 37 + i `div` stride * 71) `mod` 256)
                    1 -> if even (i + i `div` stride) then 0 else 255
                    _ -> 128
              checkPrediction size mode stride x y pixels `shouldBe` True

    it "matches frame prediction without changing any source pixels" $
      property $
        forAll (elements [8, 16]) $ \size ->
          forAll (chooseInt (0, 3)) $ \mode ->
            forAll (chooseInt (size, size * 3)) $ \width ->
              forAll (chooseInt (size, size * 3)) $ \height ->
                forAll (chooseInt (0, width - size)) $ \x ->
                  forAll (chooseInt (0, height - size)) $ \y ->
                    forAll (vectorOf (width * height) arbitrary) $ \pixels ->
                      checkPrediction size mode width x y (VS.fromList pixels)

-- Compare with the existing frame predictor, including its border rules.
-- The source immutability check protects prediction trials from changing the
-- neighbors used by later modes or by the final macroblock reconstruction.
checkPrediction :: Int -> Int -> Int -> Int -> Int -> VS.Vector Word8 -> Bool
checkPrediction size mode stride x y pixels = runST $ do
  source <- VS.thaw pixels
  reference <- VS.thaw pixels
  compact <- VSM.new (size * size)
  if size == 16
    then do
      predict16x16 mode reference stride x y
      predict16x16Block mode source stride x y compact
    else do
      predict8x8 mode reference stride x y
      predict8x8Block mode source stride x y compact
  expected <- VS.generateM (size * size) $ \i ->
    VSM.read reference ((y + i `div` size) * stride + x + i `mod` size)
  actual <- VS.freeze compact
  unchanged <- VS.freeze source
  return (actual == expected && unchanged == pixels)
