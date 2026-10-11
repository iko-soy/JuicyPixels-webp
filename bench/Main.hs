{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Comprehensive benchmarks for JuicyPixels-webp
module Main where

import Codec.Picture (DynamicImage (..), Image (..), PixelRGB8 (..), PixelRGBA8 (..), generateImage)
import Codec.Picture.Types
import Codec.Picture.WebP
import Codec.Picture.WebP.Internal.BitReader
import Codec.Picture.WebP.Internal.BitWriter
import Codec.Picture.WebP.Internal.Container
import Codec.Picture.WebP.Internal.VP8.BoolDecoder
import Codec.Picture.WebP.Internal.VP8.BoolEncoder
import Codec.Picture.WebP.Internal.VP8.ColorConvert (rgbToYCbCr, rgbToYCbCrSharp)
import Codec.Picture.WebP.Internal.VP8.DCT
import Codec.Picture.WebP.Internal.VP8.IDCT
import Codec.Picture.WebP.Internal.VP8.LoopFilter (applyLoopFilterFrame)
import Codec.Picture.WebP.Internal.VP8L.LZ77Encode (Token, lz77Compress)
import Codec.Picture.WebP.Internal.VP8L.PrefixCode (PrefixCode (..), buildPrefixCode)
import Codec.Picture.WebP.Internal.VP8L.Transform (VP8LTransform (..), applyInverseTransforms)
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Control.Monad.ST
import Criterion.Main
import Data.Bits
import qualified Data.ByteString as B
import Data.Int (Int16)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Word
import System.Directory (doesFileExist)

-- NFData instance for WebPAnimFrame (DynamicImage already has NFData from JuicyPixels)
instance NFData WebPAnimFrame where
  rnf (WebPAnimFrame img dur x y) = rnf img `seq` rnf dur `seq` rnf x `seq` rnf y

main :: IO ()
main = do
  -- Load test images
  testFiles <- loadTestFiles

  defaultMain
    [ bgroup "decode" $ decodeEndToEndBenchmarks testFiles,
      bgroup "encode" encodeEndToEndBenchmarks,
      bgroup "components" componentBenchmarks,
      bgroup "roundtrip" roundtripBenchmarks
    ]

-- | Load test files for benchmarking
loadTestFiles :: IO [(String, B.ByteString)]
loadTestFiles = do
  let testPaths =
        [ ("small", "test/data/test_simple_vp8l.webp"),
          ("gallery1-30KB", "test/data/golden/gallery1.webp"),
          ("gallery2-60KB", "test/data/golden/gallery2.webp"),
          ("gallery3-199KB", "test/data/golden/gallery3.webp"),
          ("gallery4-173KB", "test/data/golden/gallery4.webp"),
          ("gallery5-81KB", "test/data/golden/gallery5.webp")
        ]

  files <- mapM loadIfExists testPaths
  return $ concat files
  where
    loadIfExists (name, path) = do
      exists <- doesFileExist path
      if exists
        then do
          contents <- B.readFile path
          return [(name, contents)]
        else return []

-- | End-to-end decoding benchmarks
decodeEndToEndBenchmarks :: [(String, B.ByteString)] -> [Benchmark]
decodeEndToEndBenchmarks testFiles =
  [ bgroup "decodeWebP" $
      map (\(name, bs) -> bench name $ nf decodeWebP bs) testFiles,
    bgroup "parseWebP" $
      map (\(name, bs) -> bench name $ whnf parseWebP bs) testFiles
  ]

-- | End-to-end encoding benchmarks
encodeEndToEndBenchmarks :: [Benchmark]
encodeEndToEndBenchmarks =
  [ bgroup "lossless" $
      [ bench "32x32" $ nf encodeWebPLossless (generateTestImage 32 32),
        bench "64x64" $ nf encodeWebPLossless (generateTestImage 64 64),
        bench "128x128" $ nf encodeWebPLossless (generateTestImage 128 128),
        bench "256x256" $ nf encodeWebPLossless (generateTestImage 256 256),
        bench "512x512" $ nf encodeWebPLossless (generateTestImage 512 512)
      ],
    bgroup "lossless-patterns" $
      [ bench "solid-color" $ nf encodeWebPLossless (generateSolidImage 256 256 (PixelRGBA8 128 64 192 255)),
        bench "gradient" $ nf encodeWebPLossless (generateGradientImage 256 256),
        bench "checkerboard" $ nf encodeWebPLossless (generateCheckerboard 256 256),
        bench "random-noise" $ nf encodeWebPLossless (generateNoiseImage 256 256)
      ],
    bgroup "lossy" $
      [ bgroup "quality-50" $
          [ bench "32x32" $ nf (encodeWebPLossy' 50) (generateTestImageRGB 32 32),
            bench "64x64" $ nf (encodeWebPLossy' 50) (generateTestImageRGB 64 64),
            bench "128x128" $ nf (encodeWebPLossy' 50) (generateTestImageRGB 128 128),
            bench "256x256" $ nf (encodeWebPLossy' 50) (generateTestImageRGB 256 256),
            bench "512x512" $ nf (encodeWebPLossy' 50) (generateTestImageRGB 512 512)
          ],
        bgroup "quality-sweep-128x128" $
          [ bench "q10" $ nf (encodeWebPLossy' 10) (generateTestImageRGB 128 128),
            bench "q30" $ nf (encodeWebPLossy' 30) (generateTestImageRGB 128 128),
            bench "q50" $ nf (encodeWebPLossy' 50) (generateTestImageRGB 128 128),
            bench "q70" $ nf (encodeWebPLossy' 70) (generateTestImageRGB 128 128),
            bench "q90" $ nf (encodeWebPLossy' 90) (generateTestImageRGB 128 128)
          ]
      ],
    bgroup "lossy-with-alpha" $
      [ bench "128x128-q50" $ nf (encodeWebPLossyWithAlpha' 50) (generateTestImage 128 128),
        bench "256x256-q50" $ nf (encodeWebPLossyWithAlpha' 50) (generateTestImage 256 256)
      ]
  ]
  where
    encodeWebPLossy' q img = encodeWebPLossy img q
    encodeWebPLossyWithAlpha' q img = encodeWebPLossyWithAlpha img q

-- | Component-level benchmarks
componentBenchmarks :: [Benchmark]
componentBenchmarks =
  [ bgroup "bitreader" bitReaderBenchmarks,
    bgroup "bitwriter" bitWriterBenchmarks,
    bgroup "dct" dctBenchmarks,
    bgroup "idct" idctBenchmarks,
    bgroup "color-convert" colorConvertBenchmarks,
    bgroup "loop-filter" loopFilterBenchmarks,
    bgroup "bool-decoder" boolDecoderBenchmarks,
    bgroup "bool-encoder" boolEncoderBenchmarks,
    bgroup "transform" transformBenchmarks,
    bgroup "lz77" lz77Benchmarks,
    bgroup "prefix-code" prefixCodeBenchmarks
  ]

-- | BitReader benchmarks
bitReaderBenchmarks :: [Benchmark]
bitReaderBenchmarks =
  let testData = B.pack [0 .. 255]
      reader = initBitReader testData
   in [ bench "init" $ whnf initBitReader testData,
        bench "readBit-1000" $ whnf (readBitN 1000) reader,
        bench "readBits-8-100" $ whnf (readBitsN 8 100) reader,
        bench "readBits-14-100" $ whnf (readBitsN 14 100) reader,
        bench "readBits-32-50" $ whnf (readBitsN 32 50) reader
      ]
  where
    readBitN :: Int -> BitReader -> Word32
    readBitN 0 _ = 0
    readBitN n r = let (b, r') = readBit r in if b then 1 + readBitN (n - 1) r' else readBitN (n - 1) r'

    readBitsN :: Int -> Int -> BitReader -> Word32
    readBitsN _ 0 _ = 0
    readBitsN bits n r = let (v, r') = readBits bits r in v + readBitsN bits (n - 1) r'

-- | BitWriter benchmarks
bitWriterBenchmarks :: [Benchmark]
bitWriterBenchmarks =
  [ bench "writeBits-8-1000" $ nf (writeBitsN 8) 1000,
    bench "writeBits-14-1000" $ nf (writeBitsN 14) 1000,
    bench "writeBit-10000" $ nf writeBitN 10000
  ]
  where
    finalize :: BitWriter -> B.ByteString
    finalize = bitWriterToByteString . finalizeBitWriter

    writeBitsN :: Int -> Int -> B.ByteString
    writeBitsN numBits n = finalize $ go n emptyBitWriter
      where
        go 0 w = w
        go i w = go (i - 1) (writeBits numBits 0xAB w)

    writeBitN :: Int -> B.ByteString
    writeBitN n = finalize $ go n emptyBitWriter
      where
        go 0 w = w
        go i w = go (i - 1) (writeBit True w)

-- | DCT benchmarks
dctBenchmarks :: [Benchmark]
dctBenchmarks =
  [ bench "fdct4x4" $ nfIO $ runFDCT4x4 testBlock,
    bench "fdct4x4-batch-16" $ nfIO $ replicateM 16 (runFDCT4x4 testBlock),
    bench "fdct4x4-batch-256" $ nfIO $ replicateM 256 (runFDCT4x4 testBlock),
    bench "fwht4x4" $ nfIO $ runFWHT4x4 testBlock
  ]
  where
    testBlock :: VS.Vector Int16
    testBlock =
      VS.fromList
        [ 100,
          110,
          120,
          130,
          105,
          115,
          125,
          135,
          110,
          120,
          130,
          140,
          115,
          125,
          135,
          145
        ]

    runFDCT4x4 :: VS.Vector Int16 -> IO (VS.Vector Int16)
    runFDCT4x4 input = do
      result <- VS.thaw input
      stToIO $ fdct4x4 result
      VS.freeze result

    runFWHT4x4 :: VS.Vector Int16 -> IO (VS.Vector Int16)
    runFWHT4x4 input = do
      result <- VS.thaw input
      stToIO $ fwht4x4 result
      VS.freeze result

-- | IDCT benchmarks
idctBenchmarks :: [Benchmark]
idctBenchmarks =
  [ bench "idct4x4" $ nfIO $ runIDCT4x4 testCoeffs,
    bench "idct4x4-batch-16" $ nfIO $ replicateM 16 (runIDCT4x4 testCoeffs),
    bench "idct4x4-batch-256" $ nfIO $ replicateM 256 (runIDCT4x4 testCoeffs),
    bench "iwht4x4" $ nfIO $ runIWHT4x4 testCoeffs
  ]
  where
    testCoeffs :: VS.Vector Int16
    testCoeffs =
      VS.fromList
        [ 1000,
          50,
          30,
          10,
          40,
          20,
          15,
          5,
          25,
          10,
          8,
          3,
          12,
          6,
          4,
          2
        ]

    runIDCT4x4 :: VS.Vector Int16 -> IO (VS.Vector Int16)
    runIDCT4x4 input = do
      result <- VS.thaw input
      stToIO $ idct4x4 result
      VS.freeze result

    runIWHT4x4 :: VS.Vector Int16 -> IO (VS.Vector Int16)
    runIWHT4x4 input = do
      result <- VS.thaw input
      stToIO $ iwht4x4 result
      VS.freeze result

-- | Benchmark the production RGB-to-YCbCr converters, including their output
-- allocation. Inputs are prepared before timing; each call creates new planes.
colorConvertBenchmarks :: [Benchmark]
colorConvertBenchmarks =
  [ env (evaluate $ force $ generateTestImageRGB size size) $ \img ->
      bgroup
        (show size ++ "x" ++ show size)
        [ bench "box" $ nf (convertRGB False) img,
          bench "sharp" $ nf (convertRGB True) img
        ]
  | size <- [16, 256]
  ]
  where
    convertRGB :: Bool -> Image PixelRGB8 -> YUVPlanes
    convertRGB sharp img = runST $ do
      (y, u, v) <- if sharp then rgbToYCbCrSharp img else rgbToYCbCr img
      (,,) <$> VS.unsafeFreeze y <*> VS.unsafeFreeze u <*> VS.unsafeFreeze v

type YUVPlanes = (VS.Vector Word8, VS.Vector Word8, VS.Vector Word8)

type LoopFilterInput = (YUVPlanes, VU.Vector Int, VU.Vector Bool)

-- | Measure the real frame filters with visible block boundaries. Thawing the
-- immutable input on every call resets all pixels and is included in timing.
loopFilterBenchmarks :: [Benchmark]
loopFilterBenchmarks =
  [ env (evaluate $ force $ filterInput width height) $ \input ->
      bgroup
        (show width ++ "x" ++ show height)
        [ bench "normal" $ nf (filterFrame 0 width height) input,
          bench "simple" $ nf (filterFrame 1 width height) input
        ]
  | (width, height) <- [(256, 16), (256, 256)]
  ]
  where
    filterInput :: Int -> Int -> LoopFilterInput
    filterInput width height =
      let plane w h = VS.generate (w * h) $ \i ->
            fromIntegral $ 96 + 6 * (((i `mod` w) `div` 4 + (i `div` w) `div` 4) `mod` 5)
          macroblocks = (width `div` 16) * (height `div` 16)
       in ( (plane width height, plane (width `div` 2) (height `div` 2), plane (width `div` 2) (height `div` 2)),
            VU.replicate macroblocks 24,
            VU.replicate macroblocks True
          )

    filterFrame :: Int -> Int -> Int -> LoopFilterInput -> YUVPlanes
    filterFrame filterType width height ((yInput, uInput, vInput), levels, inners) = runST $ do
      y <- VS.thaw yInput
      u <- VS.thaw uInput
      v <- VS.thaw vInput
      applyLoopFilterFrame filterType 0 levels inners y width u (width `div` 2) v (width `div` 2) (height `div` 16) (width `div` 16)
      (,,) <$> VS.unsafeFreeze y <*> VS.unsafeFreeze u <*> VS.unsafeFreeze v

-- | Boolean decoder benchmarks
boolDecoderBenchmarks :: [Benchmark]
boolDecoderBenchmarks =
  let testData = B.pack $ take 1024 $ cycle [0xAB, 0xCD, 0xEF, 0x12]
   in [ bench "init" $ whnf initBoolDecoder testData,
        bench "boolRead-prob128-1000" $ whnf (readBoolN 128 1000) (initBoolDecoder testData),
        bench "boolRead-prob64-1000" $ whnf (readBoolN 64 1000) (initBoolDecoder testData),
        bench "boolRead-prob192-1000" $ whnf (readBoolN 192 1000) (initBoolDecoder testData)
      ]
  where
    readBoolN :: Word8 -> Int -> BoolDecoder -> Int
    readBoolN _ 0 _ = 0
    readBoolN prob n dec =
      let (b, dec') = boolRead prob dec
       in (if b then 1 else 0) + readBoolN prob (n - 1) dec'

-- | Boolean encoder benchmarks
boolEncoderBenchmarks :: [Benchmark]
boolEncoderBenchmarks =
  [ bench "boolWrite-prob128-1000" $ nf (writeBoolN 128) 1000,
    bench "boolWrite-prob64-1000" $ nf (writeBoolN 64) 1000
  ]
  where
    writeBoolN :: Word8 -> Int -> B.ByteString
    writeBoolN prob n = finalizeBoolEncoder $ go n initBoolEncoder
      where
        go 0 enc = enc
        go i enc = go (i - 1) (boolWrite prob True enc)

-- | Use the decoder's actual inverse transforms. applyInverseTransforms
-- creates a fresh mutable copy of the pixels for each invocation.
transformBenchmarks :: [Benchmark]
transformBenchmarks =
  [ env (evaluate $ force $ (generatePixelVector size size, predictorModes size)) $ \ ~(pixels, modes) ->
      bgroup
        (show size ++ "x" ++ show size)
        [ bench "subtract-green-inverse" $ nf (applyInverseTransforms [TransformSubGreen] size size) pixels,
          bench "predictor-vertical" $ nf (applyInverseTransforms [TransformPredictor 4 modes] size size) pixels
        ]
  | size <- [256, 512]
  ]
  where
    -- Predictor mode 2 lives in the green channel of each 16x16 tile.
    predictorModes size = VS.replicate (((size + 15) `div` 16) ^ (2 :: Int)) (2 `shiftL` 8 :: Word32)

-- | Repetitive inputs exercise maximum-length matches; the nonrepeating case
-- measures literal emission and hash-chain overhead without long matches.
lz77Benchmarks :: [Benchmark]
lz77Benchmarks =
  [ env (evaluate $ force pixels) $ \input ->
      bench name $ whnf (forceTokens . lz77Compress 256 256) input
  | (name, pixels) <-
      [ ("solid-256x256", VS.replicate (256 * 256) 0xFF806040),
        ("repeated-row-256x256", VS.generate (256 * 256) $ \i -> 0xFF000000 .|. fromIntegral (i `mod` 256)),
        ("nonrepeating-256x256", VS.generate (256 * 256) $ \i -> 0xFF000000 .|. fromIntegral i)
      ]
  ]
  where
    -- Token fields are strict, so evaluating each constructor forces the result.
    forceTokens :: V.Vector Token -> ()
    forceTokens = V.foldl' (\() token -> token `seq` ()) ()

-- | Include primary-only tables and a complete tree reaching the 15-bit limit.
prefixCodeBenchmarks :: [Benchmark]
prefixCodeBenchmarks =
  [ env (evaluate $ force lengths) $ \input ->
      bench name $ whnf (forcePrefixCode . buildPrefixCode) input
  | (name, lengths) <-
      [ ("two-symbols", VU.fromList [1, 1]),
        ("eight-bit", VU.replicate 256 8),
        ("fifteen-bit", VU.fromList ([1 .. 14] ++ [15, 15]))
      ]
  ]
  where
    forcePrefixCode :: Either String PrefixCode -> ()
    forcePrefixCode (Left err) = error err
    forcePrefixCode (Right (PrefixCodeSingle symbol)) = rnf symbol
    forcePrefixCode (Right (PrefixCodeTable table bits)) = rnf table `seq` rnf bits

generatePixelVector :: Int -> Int -> VS.Vector Word32
generatePixelVector w h = VS.generate (w * h) $ \i ->
  let x = i `mod` w
      y = i `div` w
      r = fromIntegral $ (x * 3 + y * 5) `mod` 256
      g = fromIntegral $ (x * 7 + y * 11) `mod` 256
      b = fromIntegral $ (x * 13 + y * 17) `mod` 256
      a = 255 :: Word32
   in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b

-- | Roundtrip benchmarks (encode then decode)
roundtripBenchmarks :: [Benchmark]
roundtripBenchmarks =
  [ bgroup "lossless" $
      [ bench "64x64" $ nf roundtripLossless (generateTestImage 64 64),
        bench "128x128" $ nf roundtripLossless (generateTestImage 128 128),
        bench "256x256" $ nf roundtripLossless (generateTestImage 256 256)
      ],
    bgroup "lossy-q50" $
      [ bench "64x64" $ nf (roundtripLossy 50) (generateTestImageRGB 64 64),
        bench "128x128" $ nf (roundtripLossy 50) (generateTestImageRGB 128 128),
        bench "256x256" $ nf (roundtripLossy 50) (generateTestImageRGB 256 256)
      ]
  ]
  where
    roundtripLossless :: Image PixelRGBA8 -> Either String DynamicImage
    roundtripLossless img = decodeWebP (encodeWebPLossless img)

    roundtripLossy :: Int -> Image PixelRGB8 -> Either String DynamicImage
    roundtripLossy q img = decodeWebP (encodeWebPLossy img q)

-- | Generate test RGBA image
generateTestImage :: Int -> Int -> Image PixelRGBA8
generateTestImage w h = generateImage genPixel w h
  where
    genPixel x y =
      let r = fromIntegral $ (x * 3 + y * 5) `mod` 256
          g = fromIntegral $ (x * 7 + y * 11) `mod` 256
          b = fromIntegral $ (x * 13 + y * 17) `mod` 256
       in PixelRGBA8 r g b 255

-- | Generate test RGB image
generateTestImageRGB :: Int -> Int -> Image PixelRGB8
generateTestImageRGB w h = generateImage genPixel w h
  where
    genPixel x y =
      let r = fromIntegral $ (x * 3 + y * 5) `mod` 256
          g = fromIntegral $ (x * 7 + y * 11) `mod` 256
          b = fromIntegral $ (x * 13 + y * 17) `mod` 256
       in PixelRGB8 r g b

-- | Generate solid color image
generateSolidImage :: Int -> Int -> PixelRGBA8 -> Image PixelRGBA8
generateSolidImage w h pixel = generateImage (\_ _ -> pixel) w h

-- | Generate gradient image
generateGradientImage :: Int -> Int -> Image PixelRGBA8
generateGradientImage w h = generateImage genPixel w h
  where
    genPixel x y =
      let r = fromIntegral $ (x * 255) `div` (max 1 (w - 1))
          g = fromIntegral $ (y * 255) `div` (max 1 (h - 1))
          b = fromIntegral $ ((x + y) * 127) `div` (max 1 (w + h - 2))
       in PixelRGBA8 r g b 255

-- | Generate checkerboard pattern
generateCheckerboard :: Int -> Int -> Image PixelRGBA8
generateCheckerboard w h = generateImage genPixel w h
  where
    genPixel x y =
      let isWhite = ((x `div` 16) + (y `div` 16)) `mod` 2 == 0
       in if isWhite then PixelRGBA8 255 255 255 255 else PixelRGBA8 0 0 0 255

-- | Generate noise-like pattern (deterministic)
generateNoiseImage :: Int -> Int -> Image PixelRGBA8
generateNoiseImage w h = generateImage genPixel w h
  where
    genPixel x y =
      let seed = x * 1103515245 + y * 12345 + x * y * 7919
          r = fromIntegral $ (seed `mod` 256)
          g = fromIntegral $ ((seed `div` 256) `mod` 256)
          b = fromIntegral $ ((seed `div` 65536) `mod` 256)
       in PixelRGBA8 r g b 255
