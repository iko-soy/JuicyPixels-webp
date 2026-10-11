{-# LANGUAGE OverloadedStrings #-}

module TransformSpec (spec) where

import Codec.Picture (DynamicImage (..), PixelRGBA8 (..), imageHeight, imageWidth, pixelAt)
import Codec.Picture.WebP (decodeWebP)
import Codec.Picture.WebP.Internal.VP8L.Transform
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Data.Word
import Test.Hspec

spec :: Spec
spec = describe "VP8L Transforms" $ do
  describe "Inverse Subtract Green" $ do
    it "correctly adds green to red and blue" $ do
      let pixels = VS.fromList [packARGB 255 10 20 15] -- A=255, R=10, G=20, B=15
      case applyInverseTransforms [TransformSubGreen] 1 1 pixels of
        Right result -> do
          let pixel = result VS.! 0
              (_, r, g, b) = unpackARGB pixel
          r `shouldBe` 30 -- 10 + 20 = 30
          g `shouldBe` 20 -- unchanged
          b `shouldBe` 35 -- 15 + 20 = 35
        Left err -> expectationFailure $ "Transform failed: " ++ err

    it "handles wraparound correctly" $ do
      let pixels = VS.fromList [packARGB 255 250 10 240]
      case applyInverseTransforms [TransformSubGreen] 1 1 pixels of
        Right result -> do
          let pixel = result VS.! 0
              (_, r, _, b) = unpackARGB pixel
          r `shouldBe` 4 -- (250 + 10) mod 256 = 4
          b `shouldBe` 250 -- (240 + 10) mod 256 = 250
        Left err -> expectationFailure $ "Transform failed: " ++ err

    it "processes multiple pixels" $ do
      let pixels =
            VS.fromList
              [ packARGB 255 10 5 8,
                packARGB 255 20 10 15,
                packARGB 255 30 15 20
              ]
      case applyInverseTransforms [TransformSubGreen] 3 1 pixels of
        Right result -> do
          VS.length result `shouldBe` 3
          let (_, r0, _, b0) = unpackARGB (result VS.! 0)
              (_, r1, _, b1) = unpackARGB (result VS.! 1)
              (_, r2, _, b2) = unpackARGB (result VS.! 2)
          r0 `shouldBe` 15 -- 10 + 5
          b0 `shouldBe` 13 -- 8 + 5
          r1 `shouldBe` 30 -- 20 + 10
          b1 `shouldBe` 25 -- 15 + 10
          r2 `shouldBe` 45 -- 30 + 15
          b2 `shouldBe` 35 -- 20 + 15
        Left err -> expectationFailure $ "Transform failed: " ++ err

  describe "Transform Ordering" $ do
    it "restores the original width and predictor grid before an earlier predictor" $ do
      let palette = VS.singleton (packARGB 0 0 1 0)
          modes = VS.fromList [packARGB 255 0 mode 0 | mode <- [1, 0, 2, 2, 1, 0]]
          transforms = [TransformPredictor 2 modes, TransformColorIndex palette 3]
          packed = VS.replicate 10 0 -- Two packed pixels per row, five rows.
          greens =
            [ [1, 2, 3, 4, 5, 6, 7, 8, 9],
              [2, 3, 4, 5, 1, 1, 1, 1, 10],
              [3, 4, 5, 6, 1, 1, 1, 1, 11],
              [4, 5, 6, 7, 1, 1, 1, 1, 12],
              [5, 6, 7, 8, 9, 10, 11, 12, 1]
            ]
          expected = VS.fromList [packARGB 255 0 g 0 | g <- concat greens]
      applyInverseTransforms transforms 9 5 packed `shouldBe` Right expected

    it "uses the packed width and predictor grid for a predictor after color indexing" $ do
      let palette = VS.fromList [packARGB 255 10 20 30, packARGB 255 40 50 60]
          modes = VS.fromList [packARGB 255 0 mode 0 | mode <- [1, 0]]
          transforms = [TransformColorIndex palette 3, TransformPredictor 2 modes]
          residuals = VS.replicate 10 (packARGB 0 0 1 0)
          indices =
            [ [1, 0, 0, 0, 0, 0, 0, 0, 0],
              [0, 1, 0, 0, 0, 0, 0, 0, 1],
              [1, 1, 0, 0, 0, 0, 0, 0, 0],
              [0, 0, 1, 0, 0, 0, 0, 0, 1],
              [1, 0, 1, 0, 0, 0, 0, 0, 1]
            ]
          expected = VS.fromList [palette VS.! i | i <- concat indices]
      applyInverseTransforms transforms 9 5 residuals `shouldBe` Right expected

    it "applies transforms in reverse order" $ do
      let pixels = VS.fromList [packARGB 255 100 100 100]
          transforms = [TransformSubGreen] -- Applied in reverse
      case applyInverseTransforms transforms 1 1 pixels of
        Right result -> VS.length result `shouldBe` 1
        Left err -> expectationFailure $ "Transform failed: " ++ err

    it "handles empty transform list" $ do
      let pixels = VS.fromList [packARGB 255 50 50 50]
      case applyInverseTransforms [] 1 1 pixels of
        Right result -> result `shouldBe` pixels
        Left err -> expectationFailure $ "Transform failed: " ++ err

    it "applies subtract-green on the correct side of unbundled color indexing" $ do
      let palette = VS.generate 17 $ \i -> packARGB 255 (fromIntegral i) 7 9
          indexed = VS.fromList [packARGB 255 0 0 0, packARGB 255 0 16 0]
          ci = TransformColorIndex palette 0
      applyInverseTransforms [TransformSubGreen, ci] 2 1 indexed
        `shouldBe` Right (VS.fromList [packARGB 255 7 7 16, packARGB 255 23 7 16])
      applyInverseTransforms [ci, TransformSubGreen] 2 1 indexed
        `shouldBe` Right (VS.fromList [palette VS.! 0, palette VS.! 16])

    it "expands bundled rows before undoing an earlier predictor" $ do
      let palette = VS.fromList [0, packARGB 0 1 1 1]
          -- Nine pixels require two packed bytes in each row. The unused
          -- high bits of the final byte must not carry into the next row.
          indexed = VS.fromList [0x0000AA00, 0, 0x00005500, 0x00000100]
          modes = VS.fromList [0x00000100, 0, 0x00000200]
          transforms = [TransformPredictor 2 modes, TransformColorIndex palette 3]
          gray v = packARGB 255 v v v
          expected = VS.fromList $ map gray [0, 1, 1, 2, 2, 3, 3, 4, 4, 1, 1, 2, 2, 1, 0, 1, 0, 5]
      applyInverseTransforms transforms 9 2 indexed `shouldBe` Right expected

    it "undoes a later predictor at the packed width before expanding rows" $ do
      let palette = VS.fromList [packARGB 255 10 20 30, packARGB 255 40 50 60]
          residuals = VS.fromList [0x0000AA00, 0x00005600, 0x0000AB00, 0x0000AC00]
          transforms = [TransformColorIndex palette 3, TransformPredictor 2 (VS.singleton 0x00000100)]
          indices = [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
      applyInverseTransforms transforms 9 2 residuals
        `shouldBe` Right (VS.fromList $ map (palette VS.!) indices)

    it "restores the width of a color transform before color indexing" $ do
      let color = packARGB 255 10 20 30
          palette = VS.singleton color
          coefficients = VS.fromList [0xFF000020, 0xFF000040, 0xFF000000]
          transforms = [TransformColor 2 coefficients, TransformColorIndex palette 3]
          expectedRow = replicate 4 (packARGB 255 30 20 30) ++ replicate 4 (packARGB 255 50 20 30) ++ [color]
      applyInverseTransforms transforms 9 2 (VS.replicate 4 0)
        `shouldBe` Right (VS.fromList $ expectedRow ++ expectedRow)

    it "supports color indexing between transforms at different widths" $ do
      let palette = VS.fromList [packARGB 255 10 20 30, packARGB 255 40 50 60]
          residuals = VS.fromList [0x0000AA00, 0x00005600, 0x0000AB00, 0x0000AC00]
          transforms = [TransformSubGreen, TransformColorIndex palette 3, TransformPredictor 2 (VS.singleton 0x00000100)]
          expandedPalette = VS.fromList [packARGB 255 30 20 50, packARGB 255 90 50 110]
          indices = [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
      applyInverseTransforms transforms 9 2 residuals
        `shouldBe` Right (VS.fromList $ map (expandedPalette VS.!) indices)

    it "decodes a libwebp-verified file with subtract-green before color indexing" $ do
      bytes <- B.readFile "test/data/regression/transform-order.webp"
      case decodeWebP bytes of
        Right (ImageRGBA8 img) -> do
          (imageWidth img, imageHeight img) `shouldBe` (1, 1)
          pixelAt img 0 0 `shouldBe` PixelRGBA8 15 7 16 255
        Right _ -> expectationFailure "Expected an RGBA image"
        Left err -> expectationFailure err

  describe "Multiple Pixel Processing" $ do
    it "processes 2x2 image" $ do
      let pixels =
            VS.fromList
              [ packARGB 255 10 5 10,
                packARGB 255 20 10 20,
                packARGB 255 30 15 30,
                packARGB 255 40 20 40
              ]
      case applyInverseTransforms [TransformSubGreen] 2 2 pixels of
        Right result -> do
          VS.length result `shouldBe` 4
          all (\i -> i >= 0 && i < 4) [0 .. 3] `shouldBe` True
        Left err -> expectationFailure $ "Transform failed: " ++ err

    it "processes 4x4 image" $ do
      let pixels = VS.generate 16 $ \i ->
            packARGB 255 (fromIntegral i) (fromIntegral i) (fromIntegral i)
      case applyInverseTransforms [TransformSubGreen] 4 4 pixels of
        Right result -> VS.length result `shouldBe` 16
        Left err -> expectationFailure $ "Transform failed: " ++ err

  describe "Edge Cases" $ do
    it "handles all-zero pixels" $ do
      let pixels = VS.fromList [packARGB 255 0 0 0]
      case applyInverseTransforms [TransformSubGreen] 1 1 pixels of
        Right result -> do
          let (a, r, g, b) = unpackARGB (result VS.! 0)
          [a, r, g, b] `shouldBe` [255, 0, 0, 0]
        Left err -> expectationFailure $ "Transform failed: " ++ err

    it "handles all-255 pixels" $ do
      let pixels = VS.fromList [packARGB 255 255 255 255]
      case applyInverseTransforms [TransformSubGreen] 1 1 pixels of
        Right result -> do
          let (_, r, g, b) = unpackARGB (result VS.! 0)
          g `shouldBe` 255
          r `shouldBe` 254 -- (255 + 255) mod 256 = 254
          b `shouldBe` 254 -- (255 + 255) mod 256 = 254
        Left err -> expectationFailure $ "Transform failed: " ++ err

    it "preserves alpha channel" $ do
      let pixels = VS.fromList [packARGB 128 10 20 30]
      case applyInverseTransforms [TransformSubGreen] 1 1 pixels of
        Right result -> do
          let (a, _, _, _) = unpackARGB (result VS.! 0)
          a `shouldBe` 128 -- Alpha unchanged
        Left err -> expectationFailure $ "Transform failed: " ++ err

    it "handles maximum dimension" $ do
      let size = 100
          pixels = VS.generate (size * size) $ \i ->
            packARGB 255 (fromIntegral $ i `mod` 256) 50 50
      case applyInverseTransforms [TransformSubGreen] size size pixels of
        Right result -> VS.length result `shouldBe` (size * size)
        Left err -> expectationFailure $ "Transform failed: " ++ err

-- Helper functions

packARGB :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
packARGB a r g b =
  (fromIntegral a `shiftL` 24)
    .|. (fromIntegral r `shiftL` 16)
    .|. (fromIntegral g `shiftL` 8)
    .|. fromIntegral b

unpackARGB :: Word32 -> (Word8, Word8, Word8, Word8)
unpackARGB pixel =
  ( fromIntegral ((pixel `shiftR` 24) .&. 0xFF),
    fromIntegral ((pixel `shiftR` 16) .&. 0xFF),
    fromIntegral ((pixel `shiftR` 8) .&. 0xFF),
    fromIntegral (pixel .&. 0xFF)
  )
