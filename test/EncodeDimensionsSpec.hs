module EncodeDimensionsSpec (spec) where

import Codec.Picture.Types
import Codec.Picture.WebP
import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Test.Hspec

spec :: Spec
spec = describe "Encoder dimensions" $ do
  describe "Invalid dimensions" $ do
    forM_ (invalidDimensions 16384) $ \(width, height) ->
      it ("rejects lossless " ++ show (width, height) ++ " before reading pixels") $ do
        let img = Image width height VS.empty :: Image PixelRGBA8
        evaluate (B.length $ encodeWebPLossless img)
          `shouldThrow` errorCall (dimensionError "VP8L" 16384 width height)

    forM_ (invalidDimensions 16383) $ \(width, height) ->
      it ("rejects " ++ show (width, height) ++ " in all lossy encoding paths before reading pixels") $ do
        let rgb = Image width height VS.empty :: Image PixelRGB8
            rgba = Image width height VS.empty :: Image PixelRGBA8
            expected = errorCall (dimensionError "VP8" 16383 width height)
            encodeAnimation img = encodeWebPAnimation [WebPEncodeFrame img 100 0 0] width height 100
        evaluate (B.length $ encodeWebPLossy rgb 100) `shouldThrow` expected
        evaluate (B.length $ encodeWebPLossyWithAlpha rgba 100) `shouldThrow` expected
        evaluate (B.length $ encodeAnimation $ ImageRGB8 rgb) `shouldThrow` expected
        evaluate (B.length $ encodeAnimation $ ImageRGBA8 rgba) `shouldThrow` expected

  describe "Supported boundaries" $ do
    forM_ [(1, 1), (16384, 1), (1, 16384)] $ \(width, height) ->
      it ("preserves lossless dimensions " ++ show (width, height)) $ do
        let img = generateImage (\_ _ -> PixelRGBA8 255 0 0 128) width height
        fmap imageDimensions (decodeWebP $ encodeWebPLossless img)
          `shouldBe` Right (width, height)

    forM_ [(1, 1), (16383, 1), (1, 16383)] $ \(width, height) ->
      it ("preserves dimensions " ++ show (width, height) ++ " in all lossy encoding paths") $ do
        let rgb = generateImage (\_ _ -> PixelRGB8 255 0 0) width height
            rgba = generateImage (\_ _ -> PixelRGBA8 255 0 0 128) width height
            encodeAnimation img = encodeWebPAnimation [WebPEncodeFrame img 100 0 0] width height 100
        fmap imageDimensions (decodeWebP $ encodeWebPLossy rgb 100)
          `shouldBe` Right (width, height)
        fmap imageDimensions (decodeWebP $ encodeWebPLossyWithAlpha rgba 100)
          `shouldBe` Right (width, height)
        fmap imageDimensions (decodeWebPFirstFrame $ encodeAnimation $ ImageRGB8 rgb)
          `shouldBe` Right (width, height)
        fmap imageDimensions (decodeWebPFirstFrame $ encodeAnimation $ ImageRGBA8 rgba)
          `shouldBe` Right (width, height)

invalidDimensions :: Int -> [(Int, Int)]
invalidDimensions limit =
  [(size, 1) | size <- [-1, 0, limit + 1, maxBound]]
    ++ [(1, size) | size <- [-1, 0, limit + 1, maxBound]]

dimensionError :: String -> Int -> Int -> Int -> String
dimensionError codec limit width height =
  codec ++ " encoding requires width and height in [1, " ++ show limit ++ "], got " ++ show width ++ "x" ++ show height

imageDimensions :: DynamicImage -> (Int, Int)
imageDimensions = dynamicMap (\img -> (imageWidth img, imageHeight img))
