{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of vectors.
-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}

module Clash.Hedgehog.Sized.Vector
  ( genVec
  , genNonEmptyVec
  , SVec(..)
  , genSVec
  ) where

import Prelude hiding (repeat)

import GHC.TypeLits
import Hedgehog (MonadGen, Range)
import qualified Hedgehog.Gen as Gen

import Clash.Promoted.Nat
import Clash.Sized.Vector

-- | Generate a potentially empty vector, where each element is produced
-- using the supplied generator. For a non-empty vector, see 'genNonEmptyVec'.
--
genVec :: (MonadGen m, KnownNat n) => m a -> m (Vec n a)
genVec genElem = traverse# id (repeat genElem)

-- | Generate a non-empty vector, where each element is produced using the
-- supplied generator. For a potentially empty vector, see 'genVec'.
--
genNonEmptyVec :: (MonadGen m, KnownNat n, 1 <= n) => m a -> m (Vec n a)
genNonEmptyVec = genVec

data SVec atLeast a where
  SVec :: SNat n -> Vec (atLeast + n) a -> SVec atLeast a

genSVec :: forall m atLeast a. (MonadGen m, KnownNat atLeast) => Range Word -> m a -> m (SVec atLeast a)
genSVec rangeElems genElem = do
  numExtra <- Gen.word rangeElems

  case someNatVal (toInteger numExtra) of
    Just (SomeNat proxy) ->
      let snat = snatProxy proxy
       in SVec snat <$> genVec genElem

    Nothing -> error "genSVec: Could not generate additional elements"
