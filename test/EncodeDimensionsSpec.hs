module EncodeDimensionsSpec (spec) where

import Codec.Picture.Types
import Codec.Picture.WebP
import Codec.Picture.WebP.Internal.Container
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
            encodeAnimation img = encodeWebPAnimation [WebPEncodeFrame img 100 0 0] 16383 16383 100
        evaluate (B.length $ encodeWebPLossy rgb 100) `shouldThrow` expected
        evaluate (B.length $ encodeWebPLossyWithAlpha rgba 100) `shouldThrow` expected
        evaluate (B.length $ encodeAnimation $ ImageRGB8 rgb) `shouldThrow` expected
        evaluate (B.length $ encodeAnimation $ ImageRGBA8 rgba) `shouldThrow` expected

  describe "Animation metadata" $ do
    forM_ (invalidDimensions 16777216) $ \(width, height) ->
      it ("rejects unrepresentable canvas dimensions " ++ show (width, height)) $ do
        evaluate (B.length $ encodeWebPAnimation [smallFrame 100 0 0] width height 80)
          `shouldThrow` errorCall ("Animation encoding requires canvas width and height in [1, 16777216], got " ++ show width ++ "x" ++ show height)

    forM_ [(65536, 65536), (16777216, 16777216)] $ \(width, height) ->
      it ("rejects canvas areas exceeding the format limit " ++ show (width, height)) $ do
        evaluate (B.length $ encodeWebPAnimation [smallFrame 100 0 0] width height 80)
          `shouldThrow` errorCall "Animation encoding requires canvas area at most 4294967295 pixels"

    forM_ [(-2, 0), (0, -2), (-1, 0), (0, -1), (1, 0), (0, 1)] $ \(x, y) ->
      it ("rejects negative or odd frame offsets " ++ show (x, y)) $ do
        evaluate (B.length $ encodeWebPAnimation [smallFrame 100 x y] 4 4 80)
          `shouldThrow` errorCall "Animation encoding requires nonnegative, even frame offsets"

    forM_ [(2, 0), (0, 2), (maxBound - 1, 0), (0, maxBound - 1)] $ \(x, y) ->
      it ("rejects a frame extending beyond the canvas at " ++ show (x, y)) $ do
        evaluate (B.length $ encodeWebPAnimation [smallFrame 100 x y] 2 2 80)
          `shouldThrow` errorCall "Animation frame exceeds canvas bounds"

    it "rejects a frame larger than the canvas" $ do
      let img = ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 0 0) 2 2
      evaluate (B.length $ encodeWebPAnimation [WebPEncodeFrame img 100 0 0] 1 1 80)
        `shouldThrow` errorCall "Animation frame exceeds canvas bounds"

    forM_ [-1, 16777216, minBound, maxBound] $ \duration ->
      it ("rejects an unrepresentable frame duration " ++ show duration) $ do
        evaluate (B.length $ encodeWebPAnimation [smallFrame duration 0 0] 1 1 80)
          `shouldThrow` errorCall "Animation encoding requires frame durations in [0, 16777215] milliseconds"

    forM_ [0, 16777215] $ \duration ->
      it ("preserves the frame duration boundary " ++ show duration) $ do
        let encoded = encodeWebPAnimation [smallFrame duration 0 0] 1 1 80
        fmap (map webpFrameDuration) (decodeWebPAnimation encoded) `shouldBe` Right [duration]

    forM_ [(1, 1), (16777216, 1), (1, 16777216), (65535, 65537)] $ \(width, height) ->
      it ("preserves a supported canvas and frame placement " ++ show (width, height)) $ do
        let x = (width - 1) `div` 2 * 2
            y = (height - 1) `div` 2 * 2
            encoded = encodeWebPAnimation [smallFrame 100 x y] width height 80
        case parseWebP encoded of
          Right (WebPExtended header chunks) -> do
            (vp8xCanvasWidth header, vp8xCanvasHeight header) `shouldBe` (width, height)
            [(anmfX frame, anmfY frame) | ChunkANMF frame _ <- chunks] `shouldBe` [(x, y)]
          other -> expectationFailure $ "Invalid animation container: " ++ show other

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

smallFrame :: Int -> Int -> Int -> WebPEncodeFrame
smallFrame duration x y =
  WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 0 0) 1 1) duration x y
