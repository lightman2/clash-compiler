{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Tests.BlockRam.Blob where

import Data.Bits ((.&.), (.|.), shiftL, xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder (Builder, toLazyByteString, word8, word64BE)
import Data.Word (Word64)
-- import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.TypeLits (KnownNat)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Source (HasCallStack, withFrozenCallStack)
import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)
import Test.Tasty
import Test.Tasty.Hedgehog

import Clash.Promoted.Nat (natToNum)
import Clash.Sized.Internal.BitVector (Bit(..), BitVector(..))

roundTripProperty
  :: Property
roundTripProperty = property $ do
  width <- forAll $ Gen.integral $ Range.linear 1 128
  len <- forAll $ Gen.integral $ Range.linear 0 256
  es <- forAll $ Gen.list (Range.singleton len)
                   $ Gen.integral_ $ Range.constant 0 (2 ^ width - 1)
  let (len0, runs, ends) = packAsNats width id es
  es0 <- (flip const) (len0, width, es) evalNF $ unpackNats width len0 (L.toStrict runs) (L.toStrict ends)
  diff (len, es) (==) (len0, es0)

tests :: TestTree
tests = testGroup "BlockRam"
  [ testGroup "Blob"
    [ testProperty "Round trip" roundTripProperty ]
  ]

packBVs
  :: forall n f
   . ( HasCallStack
     , Foldable f
     , KnownNat n
     )
  => Maybe Bit
  -> f (BitVector n)
  -> (Int, L.ByteString, L.ByteString)
packBVs care = packAsNats nI knownBVVal
 where
  knownBVVal (BV mask val) =
    case care of
      Just (Bit 0 0) -> val .&. (mask `xor` fullMask)
      Just (Bit 0 1) -> val .|. mask
      _ -> if mask /= 0 then
             err
           else
             val
  nI = natToNum @n @Int
  fullMask = (1 `shiftL` nI) - 1

  err = withFrozenCallStack $ error $
          "packBVs: cannot convert don't-care values. "
          ++ "Please specify mapping to definite value."

packAsNats
  :: forall a f
   . Foldable f
  => Int
  -> (a -> Natural)
  -> f a
  -> (Int, L.ByteString, L.ByteString)
packAsNats n trans es = (len0, toLazyByteString runs0, toLazyByteString ends)
 where
  (runL, endL) = n `divMod` 8
  ends | endC0 > 0 = word64BE endA0 <> ends0
       | otherwise = ends0
  (len0, runs0, ends0, endC0, endA0)
    = foldr (\a b -> go 0 (trans a) b) (0, mempty, mempty, 0, 0) es

  go :: Int -> Natural -> (Int, Builder, Builder, Int, Word64)
        -> (Int, Builder, Builder, Int, Word64)
  go runC val (len1,runs1, ends1, endC1, endA1)
    | runC < runL
      = let (val0, runB) = val `divMod` 256
            runs2 = word8 (fromIntegral runB) <> runs1
        in go (runC + 1) val0 (len1, runs2, ends1, endC1, endA1)
    | endL == 0
      = (len2, runs1, ends1, endC1, endA1)
    | endC1 + endL <= 64
      = (len2, runs1, ends1, endC1 + endL, endA2)
    | otherwise
      = (len2, runs1, ends2, endL, valEnd)
   where
    len2 = len1 + 1
    endA2 = endA1 * (2 ^ endL) + valEnd
    ends2 = word64BE endA1 <> ends1
    valEnd = fromIntegral val

unpackNats
  :: Int
  -> Int
  -> B.ByteString
  -> B.ByteString
  -> [Natural]
unpackNats _ 0   = \_ _ -> []
unpackNats n len = go 0 0 endCInit endACInit 0
 where
  (runL, endL) = n `divMod` 8
  endPerWord = 64 `div` endL
  (endCInit, endACInit) | endL == 0   = (0, 8)
                        | leader == 0 = (endPerWord, 0)
                        | otherwise   = (leader, 0)
   where
    leader = len `mod` endPerWord

  go :: Natural -> Int -> Int -> Int -> Word64 -> B.ByteString
     -> B.ByteString -> [Natural]
  go val runC endC endAC endA runs ends
    | endAC == 7
      = let Just (endB, ends0) = B.uncons ends
            endA0 = endA * 256 + fromIntegral endB
            (endA1, valEnd) = endA0 `divMod` (2 ^ endL)
            val0 = fromIntegral valEnd
        in go val0 runC endC 8 endA1 runs ends0
    | endAC < 7
      = let Just (endB, ends0) = B.uncons ends
            endA0 = endA * 256 + fromIntegral endB
        in go val runC endC (endAC + 1) endA0 runs ends0
    | runC < runL
      = let Just (runB, runs0) = B.uncons runs
            val0 = val * 256 + fromIntegral runB
        in go val0 (runC + 1) endC endAC endA runs0 ends
    | endC == 1
      = if B.null ends then
          [val]
        else
          let val0 = fromIntegral endA
          in val : go val0 0 endPerWord 0 0 runs ends
    | endL > 0
      = let (endA0, valEnd) = endA `divMod` (2 ^ endL)
            val0 = fromIntegral valEnd
            endC0 = endC - 1
        in val : go val0 0 endC0 endAC endA0 runs ends
    | B.null runs
      = [val]
    | otherwise
      = val : go 0 0 endC endAC endA runs ends
