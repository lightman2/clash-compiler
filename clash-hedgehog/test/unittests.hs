{-# LANGUAGE CPP #-}

module Main where

import Test.Tasty

#if DEBUG
import qualified Test.Clash.Hedgehog.Core.Type
#endif

tests :: TestTree
tests = testGroup "Unittests"
  [
#if DEBUG
    Test.Clash.Hedgehog.Core.Type.tests
#endif
  ]

main :: IO ()
main = defaultMain tests
