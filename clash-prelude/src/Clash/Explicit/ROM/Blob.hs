{-|
Copyright  :  (C) 2021     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.ROM.Blob
  ( romBlob
  , MemBlob
  , createMemBlob
  , romBlob#
  , unpackMemBlob
  ) where

import Data.Array             (listArray)
import Data.Array.Base        (unsafeAt)
import GHC.Stack              (HasCallStack, withFrozenCallStack)
import GHC.TypeLits           (KnownNat)

import Clash.Explicit.BlockRam.Blob (createMemBlob, MemBlob, unpackMemBlob)
import Clash.Promoted.Nat (natToNum)
import Clash.Signal.Internal
  (Clock (..), KnownDomain, Signal (..), Enable, fromEnable)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.XException       (deepErrorX, seqX)

romBlob
  :: forall dom n m addr
   . ( KnownDomain dom
     , KnownNat n
     , KnownNat m
     , Enum addr
     , HasCallStack
     )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -> MemBlob n m
  -- ^ ROM content
  -> Signal dom addr
  -- ^ Read address @rd@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romBlob = \clk en content rd ->
  withFrozenCallStack (romBlob# clk en content (fromEnum <$> rd))
{-# INLINE romBlob #-}

-- | ROM primitive
romBlob#
  :: forall dom n m
   . ( KnownDomain dom
     , KnownNat n
     , KnownNat m
     , HasCallStack
     )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -> MemBlob n m
  -- ^ ROM content
  -> Signal dom Int
  -- ^ Read address @rd@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @rd@ from the previous clock cycle
romBlob# !_ en content =
  go
    (withFrozenCallStack (deepErrorX "rom: initial value undefined"))
    (fromEnable en)
 where
  szI = natToNum @n @Int
  arr = listArray (0,szI-1) $ unpackMemBlob content

  go o (e :- es) rd@(~(r :- rs)) =
    let o1 = if e then safeAt r else o
    -- See [Note: register strictness annotations]
    in  o `seqX` o :- (rd `seq` go o1 es rs)

  safeAt :: Int -> BitVector m
  safeAt i =
    if (0 <= i) && (i < szI) then
      unsafeAt arr i
    else
      withFrozenCallStack
        (deepErrorX ("rom: address " ++ show i ++
                     "not in range [0.." ++ show szI ++ ")"))
  {-# INLINE safeAt #-}
{-# NOINLINE romBlob# #-}
-- {-# ANN romBlob# hasBlackBox #-}
