{-|
Copyright  :  (C) 2021     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- {-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
-- {-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
-- {-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.BlockRam.Blob
  ( MemBlob
  , createMemBlob
  , unpackMemBlob
  ) where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Stack (HasCallStack) -- , withFrozenCallStack)
import GHC.TypeLits (KnownNat)
import Language.Haskell.TH
  (Dec, litE, litT, mkName, normalB, numTyLit, Q, sigD, stringL, valD, varP)

import Clash.Explicit.BlockRam.Internal (MemBlob(..), packBVs, unpackNats)
import Clash.Promoted.Nat (natToInteger, natToNum, SNat(..))
import Clash.Sized.Internal.BitVector (Bit(..), BitVector(..))

createMemBlob
  :: forall m f
   . ( HasCallStack
     , Foldable f
     , KnownNat m
     )
  => String
  -> Maybe Bit
  -> f (BitVector m)
  -> Q [Dec]
createMemBlob name care es = sequence
  [ sigD name0 [t| MemBlob $(n) $(m) |]
  , valD (varP name0) (normalB [| MemBlob SNat SNat $(runs) $(ends) |]) []
  ]
 where
  name0 = mkName name
  n = litT . numTyLit $ toInteger len
  m = litT . numTyLit $ natToInteger @m
  runs = litE . stringL . L8.unpack $ B64L.encode runsB
  ends = litE . stringL . L8.unpack $ B64L.encode endsB
  (len, runsB, endsB) = packBVs care es

unpackMemBlob
  :: forall n m
   . MemBlob n m
  -> [BitVector m]
unpackMemBlob (MemBlob SNat SNat runs ends)
  = map (BV 0) $
      unpackNats (natToNum @m) (natToNum @n) (decode runs) (decode ends)
 where
  decode = (\(Right bs) -> bs) . B64.decode . B8.pack
