{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of Unsigned numbers.
-}

module Clash.Hedgehog.Sized.Unsigned
  ( genUnsigned
  ) where

import GHC.TypeLits (KnownNat)
import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen

import Clash.Sized.Internal.Unsigned

genUnsigned :: (MonadGen m, KnownNat n) => Range (Unsigned n) -> m (Unsigned n)
genUnsigned = Gen.integral
