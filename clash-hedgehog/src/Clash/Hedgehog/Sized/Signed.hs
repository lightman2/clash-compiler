{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of Signed numbers.
-}

module Clash.Hedgehog.Sized.Signed
  ( genSigned
  ) where

import GHC.TypeLits (KnownNat)
import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen

import Clash.Sized.Internal.Signed

genSigned :: (MonadGen m, KnownNat n) => Range (Signed n) -> m (Signed n)
genSigned = Gen.integral
