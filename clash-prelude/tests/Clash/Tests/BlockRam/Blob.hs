--{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
--{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
--{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Tests.BlockRam.Blob where

import qualified Data.ByteString.Lazy as L
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Clash.Explicit.BlockRam.Internal (packAsNats, unpackNats)

roundTripProperty
  :: Property
roundTripProperty = property $ do
  width <- forAll $ Gen.integral $ Range.linear 1 128
  len <- forAll $ Gen.integral $ Range.linear 0 256
  es <- forAll $ Gen.list (Range.singleton len)
                   $ Gen.integral_ $ Range.constant 0 (2 ^ width - 1)
  let (len0, runs, ends) = packAsNats width id es
      es0 = take 300 $
              unpackNats width len0 (L.toStrict runs) (L.toStrict ends)
  diff (len, es) (==) (len0, es0)

tests :: TestTree
tests = testGroup "BlockRam"
  [ testGroup "Blob"
    [ testProperty "Round trip" roundTripProperty ]
  ]
