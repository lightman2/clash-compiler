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

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.BlockRam.Blob
  ( MemBlob
  , createMemBlob
  , unpackMemBlob
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Unsafe as B
import GHC.Exts (Addr#)
import GHC.Stack (HasCallStack) -- , withFrozenCallStack)
import GHC.TypeLits (KnownNat)
import Language.Haskell.TH
  (Dec, integerL, litE, litT, mkName, normalB, numTyLit, Q, sigD, stringPrimL,
   valD, varP)
import System.IO.Unsafe (unsafePerformIO)

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
  , valD (varP name0) (normalB [| MemBlob SNat SNat $(runsLen) $(runs)
                                  $(endsLen) $(ends) |]) []
  ]
 where
  name0 = mkName name
  n = litT . numTyLit $ toInteger len
  m = litT . numTyLit $ natToInteger @m
  runsLen = litE . integerL . toInteger $ L.length runsB
  runs = litE . stringPrimL $ L.unpack runsB
  endsLen = litE . integerL . toInteger $ L.length endsB
  ends = litE . stringPrimL $ L.unpack endsB
  (len, runsB, endsB) = packBVs care es

unpackMemBlob
  :: forall n m
   . MemBlob n m
  -> [BitVector m]
unpackMemBlob (MemBlob SNat SNat runsLen runs endsLen ends)
  = map (BV 0) $
      unpackNats (natToNum @m) (natToNum @n) runsB endsB
 where
  runsB = unsafePerformIO $ B.unsafePackAddressLen runsLen runs
  endsB = unsafePerformIO $ B.unsafePackAddressLen endsLen ends
