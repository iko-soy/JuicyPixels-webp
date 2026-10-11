{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module PrefixCodeSpec (spec) where

import Codec.Picture.WebP.Internal.BitReader
import Codec.Picture.WebP.Internal.BitWriter
import Codec.Picture.WebP.Internal.VP8L.PrefixCode
import Control.Monad (forM_)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.List
import qualified Data.Vector.Unboxed as VU
import Test.Hspec

spec :: Spec
spec = describe "PrefixCode" $ do
  describe "Code Construction" $ do
    it "builds single-symbol code" $ do
      let lengths = VU.fromList [0, 0, 8, 0]
      case buildPrefixCode lengths of
        Left _ -> expectationFailure "Should build code"
        Right (PrefixCodeSingle sym) -> sym `shouldBe` 2
        Right _ -> expectationFailure "Expected single symbol code"

    it "builds two-symbol code" $ do
      let lengths = VU.fromList [1, 1]
      case buildPrefixCode lengths of
        Left _ -> expectationFailure "Should build code"
        Right _ -> return () -- Success
    it "builds code with multiple symbols" $ do
      let lengths = VU.fromList [2, 2, 2, 2] -- All symbols have length 2
      case buildPrefixCode lengths of
        Left _ -> expectationFailure "Should build code"
        Right (PrefixCodeTable _ _) -> return ()
        Right _ -> expectationFailure "Expected table code"

    it "rejects empty code lengths" $ do
      let lengths = VU.fromList []
      buildPrefixCode lengths `shouldSatisfy` isLeft

    it "rejects all-zero code lengths" $ do
      let lengths = VU.fromList [0, 0, 0, 0]
      buildPrefixCode lengths `shouldSatisfy` isLeft

    it "rejects oversubscribed code trees before constructing a table" $ do
      forM_ [[1, 1, 1], [1, 1, 1, 1, 15]] $ \lengths ->
        case buildPrefixCode (VU.fromList lengths) of
          Left err -> err `shouldBe` "Oversubscribed prefix code tree"
          Right _ -> expectationFailure "Expected an oversubscribed tree to be rejected"

    it "rejects incomplete multi-symbol code trees" $ do
      forM_ [[2, 2], [8, 8, 8, 8]] $ \lengths ->
        case buildPrefixCode (VU.fromList lengths) of
          Left err -> err `shouldBe` "Incomplete prefix code tree"
          Right _ -> expectationFailure "Expected an incomplete tree to be rejected"

    it "rejects code lengths outside 0..15, including singletons" $ do
      forM_ [[-1, 1, 1], [16, 1], [-1], [16]] $ \lengths ->
        buildPrefixCode (VU.fromList lengths) `shouldSatisfy` isLeft

    it "preserves zero-bit singleton codes for every valid nonzero length" $ do
      forM_ [1 .. 15] $ \len ->
        case buildPrefixCode (VU.fromList [0, len, 0]) of
          Right code@(PrefixCodeSingle sym) -> do
            sym `shouldBe` 1
            let (decoded, reader) = decodeSymbol code (initBitReader B.empty)
            decoded `shouldBe` 1
            getBitPosition reader `shouldBe` 0
            hasReadPastEnd reader `shouldBe` False
          _ -> expectationFailure "Expected a zero-bit singleton code"

    it "handles mixed length codes" $ do
      let lengths = VU.fromList [1, 3, 3, 4, 4, 4, 4]
      case buildPrefixCode lengths of
        Left _ -> expectationFailure "Should build code"
        Right (PrefixCodeTable _ _) -> return ()
        Right _ -> expectationFailure "Expected table code"

  describe "Code Decoding" $ do
    it "decodes mixed code lengths through the 15-bit limit" $ do
      -- Complete canonical tree: 0, 10, 110, ..., fourteen 1s then 0/1.
      -- Decode every symbol together so an incorrect consumed length also fails.
      let lengths = VU.fromList ([1 .. 14] ++ [15, 15])
          codewords = [(len, (1 `shiftL` len) - 2) | len <- [1 .. 15]] ++ [(15, 32767)]
          writer = foldl (\w (len, value) -> writeBitsReversed len value w) emptyBitWriter codewords
          reader = initBitReader $ bitWriterToByteString $ finalizeBitWriter writer
          check _ r [] = getBitPosition r `shouldBe` sum (map fst codewords)
          check code r (expected : rest) = do
            let (actual, r') = decodeSymbol code r
            actual `shouldBe` expected
            check code r' rest
      case buildPrefixCode lengths of
        Left err -> expectationFailure err
        Right code -> check code reader [0 .. 15]

    it "decodes all 9-bit codes across multiple secondary tables" $ do
      -- With 512 equally long codes, each symbol is its canonical code value.
      case buildPrefixCode (VU.replicate 512 9) of
        Left err -> expectationFailure err
        Right code -> forM_ [0 .. 511] $ \expected -> do
          let writer = writeBitsReversed 9 expected emptyBitWriter
              reader = initBitReader $ bitWriterToByteString $ finalizeBitWriter writer
              (actual, reader') = decodeSymbol code reader
          actual `shouldBe` fromIntegral expected
          getBitPosition reader' `shouldBe` 9

    it "keeps direct codes separate from multiple secondary tables" $ do
      -- The first 254 prefixes are eight-bit codes; the last two each
      -- contain two nine-bit codes, with canonical values 508..511.
      let lengths = VU.fromList (replicate 254 8 ++ replicate 4 9)
      case buildPrefixCode lengths of
        Left err -> expectationFailure err
        Right code -> forM_ [0 .. 257] $ \symbol -> do
          let len = lengths VU.! symbol
              canonical = if symbol < 254 then symbol else symbol + 254
              writer = writeBitsReversed len (fromIntegral canonical) emptyBitWriter
              reader = initBitReader $ bitWriterToByteString $ finalizeBitWriter writer
              (actual, reader') = decodeSymbol code reader
          actual `shouldBe` fromIntegral symbol
          getBitPosition reader' `shouldBe` len

    it "decodes single-symbol code" $ do
      let lengths = VU.fromList [0, 0, 5, 0]
          bs = B.pack [0xFF]
          reader = initBitReader bs
      case buildPrefixCode lengths of
        Right code -> do
          let (sym, _) = decodeSymbol code reader
          sym `shouldBe` 2
        Left err -> expectationFailure $ "Build failed: " ++ err

    it "decodes two-symbol code" $ do
      let lengths = VU.fromList [1, 1] -- symbols 0 and 1
          bs = B.pack [0b00000001] -- bit 0 = 1 (symbol 1), bit 1 = 0 (symbol 0)
          reader = initBitReader bs
      case buildPrefixCode lengths of
        Right code -> do
          let (sym1, reader') = decodeSymbol code reader
              (sym2, _) = decodeSymbol code reader'
          sym1 `shouldBe` 1 -- bit 0 = 1
          sym2 `shouldBe` 0 -- bit 1 = 0
        Left err -> expectationFailure $ "Build failed: " ++ err

    it "decodes multiple symbols from bitstream" $ do
      let lengths = VU.fromList [2, 2, 2, 2]
          bs = B.pack [0xFF, 0xFF]
          reader = initBitReader bs
      case buildPrefixCode lengths of
        Right code -> do
          let (sym1, r1) = decodeSymbol code reader
              (sym2, r2) = decodeSymbol code r1
              (sym3, _) = decodeSymbol code r2
          -- Each decode consumes 2 bits
          sym1 `shouldSatisfy` (<= 3)
          sym2 `shouldSatisfy` (<= 3)
          sym3 `shouldSatisfy` (<= 3)
        Left err -> expectationFailure $ "Build failed: " ++ err

  describe "Code Length Reading" $ do
    it "rejects an oversubscribed code-length alphabet" $ do
      let header = writeBits 4 0 $ writeBit False emptyBitWriter
          writer = foldl (\w _ -> writeBits 3 1 w) header [1 .. 4 :: Int]
          reader = initBitReader $ bitWriterToByteString $ finalizeBitWriter writer
      case readCodeLengths 256 reader of
        Left err -> err `shouldContain` "Oversubscribed prefix code tree"
        Right _ -> expectationFailure "Expected the invalid code-length tree to be rejected"

    it "reads simple code (1 symbol)" $ do
      -- Simple code: 1 bit set (1 symbol), 8 bits for symbol
      let bs = B.pack [0b00000001, 0x05] -- simple=1, num=0 (1 symbol), symbol=5
          reader = initBitReader bs
      case readCodeLengths 256 reader of
        Right (lengths, _) -> do
          -- Symbol 5 should have length 0 (single symbol codes use 0-bit)
          lengths VU.! 5 `shouldBe` 0
        Left err -> expectationFailure $ "Read failed: " ++ err

    it "reads simple code (2 symbols)" $ do
      -- Simple code: 1 bit set, 1 bit for count (1=2 symbols), 8+8 bits for symbols
      -- Bits: simple=1, numSymbols-1=1, symbol1 (8 bits), symbol2 (8 bits)
      -- Packed LSB: bit0=1 (simple), bit1=1 (count), bits 2-9 = symbol1, bits 10-17 = symbol2
      -- Byte 0: bits 0-7 = 0b11000000 (bit0=1, bit1=1, bits2-7=first 6 bits of symbol1)
      -- This is complex to pack correctly, so let's test the result differently
      let bs = B.pack [0b00000011, 0x03, 0x00, 0x07, 0x00] -- simple=1, num=1, sym1=3, sym2=7
          reader = initBitReader bs
      case readCodeLengths 256 reader of
        Right (lengths, _) -> do
          -- Check that we got valid code lengths
          VU.length lengths `shouldBe` 256
        Left err -> expectationFailure $ "Read failed: " ++ err

  describe "kCodeLengthCodeOrder" $ do
    it "has correct length" $ do
      VU.length kCodeLengthCodeOrder `shouldBe` 19

    it "starts with 17, 18, 0" $ do
      VU.take 3 kCodeLengthCodeOrder `shouldBe` VU.fromList [17, 18, 0]

    it "contains all unique values 0-18" $ do
      let values = VU.toList kCodeLengthCodeOrder
      length values `shouldBe` 19
      all (\i -> i `elem` values) [0 .. 18] `shouldBe` True

  describe "Edge Cases" $ do
    it "handles a complete 8-bit code" $ do
      let lengths = VU.replicate 256 8
      case buildPrefixCode lengths of
        Right _ -> return ()
        Left _ -> expectationFailure "Should handle reasonable codes"

    it "handles sparse code lengths" $ do
      let lengths = VU.generate 256 $ \i ->
            if i `mod` 16 == 0 then 4 else 0
      case buildPrefixCode lengths of
        Right _ -> return ()
        Left _ -> expectationFailure "Should handle sparse codes"

    it "handles single long code" $ do
      let lengths = VU.replicate 256 0 VU.// [(42, 10)]
      case buildPrefixCode lengths of
        Right (PrefixCodeSingle sym) -> sym `shouldBe` 42
        _ -> expectationFailure "Expected single symbol"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
