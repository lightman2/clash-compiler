{-# LANGUAGE TypeApplications #-}

-- {-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
-- {-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
-- {-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Explicit.BlockRam.Internal where

import Data.Bits ((.&.), (.|.), shiftL, xor)
import qualified Data.ByteString as B
-- import qualified Data.ByteString.Base64 as B64
-- import qualified Data.ByteString.Base64.Lazy as B64L
-- import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Lazy.Char8 as L8
import Data.ByteString.Builder (Builder, toLazyByteString, word8, word64BE)
import Data.Word (Word64)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.TypeLits (KnownNat)
import Numeric.Natural (Natural)

import Clash.Promoted.Nat (natToNum, SNat(..))
import Clash.Sized.Internal.BitVector (Bit(..), BitVector(..))

data MemBlob n m = MemBlob
  { memBlobLength :: !(SNat n)
  , memBlobWidth :: !(SNat m)
  , memBlobRuns :: !String
  , memBlobEnds :: !String
  }

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
  knownBVVal (BV 0    val) = val
  knownBVVal (BV mask val) = case care of
                               Just (Bit 0 0) -> val .&. (mask `xor` fullMask)
                               Just (Bit 0 1) -> val .|. mask
                               _ -> err
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
    | endC2 <= 64
      = (len2, runs1, ends1, endC2, endA2)
    | otherwise
      = (len2, runs1, ends2, endL, valEnd)
   where
    len2 = len1 + 1
    endC2 = endC1 + endL
    endA2 = endA1 * (2 ^ endL) + valEnd
    ends2 = word64BE endA1 <> ends1
    valEnd = fromIntegral val

unpackNats
  :: Int
  -> Int
  -> B.ByteString
  -> B.ByteString
  -> [Natural]
unpackNats _ 0 _ _ = []
unpackNats n len runBs endBs
  | n < 8     = ends
  | otherwise = go (head ends) runL runBs (tail ends)
 where
  (runL, endL) = n `divMod` 8
  ends = if endL == 0 then
           repeat 0
         else
           unpackEnds endL len $ unpackW64s endBs

  go val 0    runBs0 ~(end0:ends0) = val : go end0 runL runBs0 ends0
  go _   _    runBs0 _             | B.null runBs0 = []
  go val runC runBs0 ends0
    = let Just (runB, runBs1) = B.uncons runBs0
          val0 = val * 256 + fromIntegral runB
      in go val0 (runC - 1) runBs1 ends0

unpackW64s
  :: B.ByteString
  -> [Word64]
unpackW64s = go 8 0
 where
  go :: Int -> Word64 -> B.ByteString -> [Word64]
  go 8 _   endBs | B.null endBs = []
  go 0 val endBs = val : go 8 0 endBs
  go n val endBs = let Just (endB, endBs0) = B.uncons endBs
                       val0 = val * 256 + fromIntegral endB
                   in go (n - 1) val0 endBs0

unpackEnds
  :: Int
  -> Int
  -> [Word64]
  -> [Natural]
unpackEnds _    _   []     = []
unpackEnds endL len (w:ws) = go endCInit w ws
 where
  endPerWord = 64 `div` endL
  leader = len `mod` endPerWord
  endCInit | leader == 0 = endPerWord
           | otherwise   = leader

  go 0 _    []       = []
  go 0 _    (w0:ws0) = go endPerWord w0 ws0
  go n endA ws0      = let (endA0, valEnd) = endA `divMod` (2 ^ endL)
                       in fromIntegral valEnd : go (n-1) endA0 ws0
