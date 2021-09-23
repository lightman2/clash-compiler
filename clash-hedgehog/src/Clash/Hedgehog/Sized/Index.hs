{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of Index.
-}

module Clash.Hedgehog.Sized.Index
  ( genIndex
  ) where

import GHC.TypeLits (KnownNat)
import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen

import Clash.Sized.Internal.Index

genIndex :: (MonadGen m, KnownNat n) => Range (Index n) -> m (Index n)
genIndex = Gen.integral
