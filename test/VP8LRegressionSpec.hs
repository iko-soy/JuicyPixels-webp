module VP8LRegressionSpec (spec) where

import Codec.Picture.Types
import Codec.Picture.WebP (decodeWebP)
import Codec.Picture.WebP.Internal.Container
import Codec.Picture.WebP.Internal.Encode (makeRIFFContainer, makeVP8LChunk)
import Codec.Picture.WebP.Internal.VP8L (decodeVP8LHeaderless)
import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Test.Hspec

spec :: Spec
spec = describe "VP8L decoder regressions" $ do
  -- Independently hand-packed 1x1 streams. libwebp rejects every fixture;
  -- their payloads differ only in the repeated transform sequence.
  forM_ [("predictor", 0), ("color", 1), ("subgreen", 2), ("palette", 3), ("separated", 2)] $ \(name, transformType) ->
    it ("rejects duplicate transform types: " ++ name) $ do
      input <- B.readFile ("test/data/regression/vp8l-duplicate-" ++ name ++ ".webp")
      let expected = Left ("Duplicate VP8L transform type: " ++ show (transformType :: Int))
      fmap (const ()) (decodeWebP input) `shouldBe` expected
      -- ALPH uses this same transform syntax without the five-byte VP8L header.
      fmap (const ()) (decodeVP8LHeaderless 1 1 (B.drop 25 input)) `shouldBe` expected

  it "allows all four distinct transforms in stream order" $ do
    -- Subtract-green, predictor, palette, color: verified with libwebp.
    input <- B.readFile "test/data/regression/vp8l-unique-transforms.webp"
    case decodeWebP input of
      Right (ImageRGBA8 image) -> pixelAt image 0 0 `shouldBe` PixelRGBA8 30 20 50 254
      Left err -> expectationFailure err
      Right _ -> expectationFailure "Expected RGBA output"

  it "decodes a complete Huffman tree containing 9-bit codes" $ do
    input <- B.readFile "test/data/regression/vp8l-long-code.webp"
    case decodeWebP input of
      Right (ImageRGBA8 image) -> do
        imageWidth image `shouldBe` 1
        imageHeight image `shouldBe` 1
        VS.toList (imageData image) `shouldBe` [0, 8, 0, 255]
      Left err -> expectationFailure err
      Right _ -> expectationFailure "Expected RGBA output"

  forM_ ["first", "second"] $ \position -> do
    let fixture = "test/data/regression/vp8l-unused-distance-" ++ position ++ ".webp"
    it ("ignores an unused " ++ position ++ " simple distance symbol without consuming copy bits") $ do
      input <- B.readFile fixture
      case decodeWebP input of
        Right (ImageRGBA8 image) -> do
          (imageWidth image, imageHeight image) `shouldBe` (3, 1)
          VS.toList (imageData image) `shouldBe` concatMap (\green -> [0, green, 0, 255]) [8, 8, 9]
        Left err -> expectationFailure err
        Right _ -> expectationFailure "Expected RGBA output"

    it ("rejects truncated data with an unused " ++ position ++ " distance symbol") $ do
      input <- B.readFile fixture
      case parseWebP input of
        Right (WebPSimpleLossless payload) -> do
          let chunk = makeVP8LChunk (B.take (B.length payload - 2) payload)
              truncated = makeRIFFContainer (fromIntegral $ B.length chunk) chunk
          fmap (const ()) (decodeWebP truncated) `shouldBe` Left "Unexpected end of VP8L bitstream"
        _ -> expectationFailure "Expected a simple lossless fixture"

  it "selects spatial Huffman groups at the encoded block size" $ do
    input <- B.readFile "test/data/regression/vp8l-meta-prefix.webp"
    case decodeWebP input of
      Right (ImageRGBA8 image) -> do
        imageWidth image `shouldBe` 8
        imageHeight image `shouldBe` 1
        VS.toList (imageData image)
          `shouldBe` concatMap (\green -> [0, green, 0, 255]) [10, 10, 10, 10, 20, 20, 20, 20]
      Left err -> expectationFailure err
      Right _ -> expectationFailure "Expected RGBA output"

  it "returns Left for a backreference before the first pixel" $ do
    input <- B.readFile "test/data/regression/vp8l-invalid-backref.webp"
    case decodeWebP input of
      Left err -> err `shouldContain` "Distance 1 exceeds position 0"
      Right _ -> expectationFailure "Expected invalid backreference to be rejected"

  it "returns Left for an oversubscribed Huffman tree" $ do
    input <- B.readFile "test/data/regression/vp8l-oversubscribed.webp"
    case decodeWebP input of
      Left err -> err `shouldContain` "Oversubscribed prefix code tree"
      Right _ -> expectationFailure "Expected an oversubscribed tree to be rejected"

  it "returns Left for an incomplete Huffman tree" $ do
    input <- B.readFile "test/data/regression/vp8l-incomplete.webp"
    case decodeWebP input of
      Left err -> err `shouldContain` "Incomplete prefix code tree"
      Right _ -> expectationFailure "Expected an incomplete tree to be rejected"

  it "rejects a backreference that extends past the final pixel" $ do
    input <- B.readFile "test/data/regression/vp8l-copy-overrun.webp"
    case decodeWebP input of
      Left err -> err `shouldContain` "Backreference length 2 exceeds remaining pixels 1"
      Right _ -> expectationFailure "Expected an overlong backreference to be rejected"

  it "accepts a backreference that ends exactly at the final pixel" $ do
    input <- B.readFile "test/data/regression/vp8l-copy-exact.webp"
    case decodeWebP input of
      Right (ImageRGBA8 image) -> do
        imageWidth image `shouldBe` 3
        imageHeight image `shouldBe` 1
        VS.toList (imageData image) `shouldBe` concat (replicate 3 [0, 0, 0, 255])
      Left err -> expectationFailure err
      Right _ -> expectationFailure "Expected RGBA output"
