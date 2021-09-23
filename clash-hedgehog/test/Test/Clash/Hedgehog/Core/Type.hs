{-# LANGUAGE OverloadedStrings #-}

module Test.Clash.Hedgehog.Core.Type (tests) where

import Hedgehog
import Hedgehog.Gen
import Test.Tasty
import Test.Tasty.Hedgehog

import Clash.Core.HasType (inferCoreKindOf)
import Clash.Core.Subst () -- Eq Type
import Clash.Core.TysPrim (liftedTypeKind, tysPrimMap)

import Clash.Hedgehog.Core.Monad
import Clash.Hedgehog.Core.Type

tests :: TestTree
tests =
  testGroup "Clash.Hedgehog.Core.Type"
    [ testGroup "genKind"
        [ testProperty "Has requested kind" genKindHasKind
        ]
    , testGroup "genType"
        [ testProperty "Has requested type" genTypeHasKind
        ]
    ]

genCoreGenConfig :: Gen CoreGenConfig
genCoreGenConfig =
  CoreGenConfig
    <$> bool
    <*> bool
    <*> bool
    <*> bool
    <*> bool

genKindHasKind :: Property
genKindHasKind =
  property $ do
    -- We want to know if any configurations are routinely biased against.
    config <- forAll genCoreGenConfig
    collect config

    kn <- forAll (runCoreGenT (genKindFrom tysPrimMap mempty liftedTypeKind) config)
    liftedTypeKind === inferCoreKindOf tysPrimMap kn

-- TODO This routinely fails because if we have a kind
--
--   Type -> Type
--
-- and generate a type for it, we may end up with something like
--
--   forall (a :: Type). a -> a
--
-- which inferCoreKindOf will say has the kind Type.
genTypeHasKind :: Property
genTypeHasKind =
  property $ do
    config <- forAll genCoreGenConfig
    collect config

    (kn, ty) <- forAll $ flip runCoreGenT config $ do
      kn <- genKindFrom tysPrimMap mempty liftedTypeKind
      ty <- genPolyTypeFrom tysPrimMap mempty kn

      pure (kn, ty)

    kn === inferCoreKindOf tysPrimMap ty

