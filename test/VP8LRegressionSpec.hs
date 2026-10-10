module VP8LRegressionSpec (spec) where

import Codec.Picture.Types
import Codec.Picture.WebP (decodeWebP)
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Test.Hspec

spec :: Spec
spec = describe "VP8L decoder regressions" $ do
  it "decodes a complete Huffman tree containing 9-bit codes" $ do
    input <- B.readFile "test/data/regression/vp8l-long-code.webp"
    case decodeWebP input of
      Right (ImageRGBA8 image) -> do
        imageWidth image `shouldBe` 1
        imageHeight image `shouldBe` 1
        VS.toList (imageData image) `shouldBe` [0, 8, 0, 255]
      Left err -> expectationFailure err
      Right _ -> expectationFailure "Expected RGBA output"

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
