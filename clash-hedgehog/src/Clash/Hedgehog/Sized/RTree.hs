{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of RTree.
-}

module Clash.Hedgehog.Sized.RTree
  ( genRTree
  , genNonEmptyRTree
  ) where

import GHC.TypeLits (KnownNat, type (<=))
import Hedgehog (MonadGen)

import Clash.Sized.RTree

genRTree :: (MonadGen m, KnownNat n) => m a -> m (RTree n a)
genRTree genElem = sequenceA (trepeat genElem)

genNonEmptyRTree :: (MonadGen m, KnownNat n, 1 <= n) => m a -> m (RTree n a)
genNonEmptyRTree = genRTree
