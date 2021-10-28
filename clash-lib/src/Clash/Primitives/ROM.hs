{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Primitives.ROM where

import Prelude

import Control.Monad.State (State)
import Data.Maybe (isJust, fromJust)
import Data.String (fromString, IsString)
import Data.String.Interpolate (i)

import Clash.Backend (Backend)
import Clash.Netlist.Types
  (BlackBoxContext(..), Expr(..), HWType(..), Literal(..), Modifier(..),
   TemplateFunction(..))
import Data.Text.Prettyprint.Doc.Extra (Doc)

import Clash.Explicit.BlockRam.Internal (MemBlob(..))

romBlobTF :: TemplateFunction
romBlobTF =
  TemplateFunction used valid romBlobTemplate
 where
  used = [6]
  valid = const True

romBlobTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
romBlobTemplate bbCtx = pure bbText
 where
  blob = bbInputs bbCtx !! 6
  bbText = fromString $ show blob
  
