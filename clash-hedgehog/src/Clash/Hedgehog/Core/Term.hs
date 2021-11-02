{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random, type-directed generation of Term.
-}

{-# LANGUAGE TupleSections #-}

module Clash.Hedgehog.Core.Term
  ( genTermFrom
  ) where

import Control.Monad (forM)
import qualified Data.Set as Set
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Core.DataCon
import Clash.Core.HasType
import Clash.Core.Pretty (showPpr)
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.TysPrim (liftedTypeKind, typeSymbolKind)
import Clash.Core.Util (undefinedTy)
import Clash.Core.Var
import Clash.Unique

import Clash.Hedgehog.Core.Literal
import Clash.Hedgehog.Core.Monad
import Clash.Hedgehog.Core.Name
import Clash.Hedgehog.Core.Type
import Clash.Hedgehog.Core.Var
import Clash.Hedgehog.Unique

-- | Sample a data constructor from the environment, potentially partially
-- applying it so that the type fits the hole. If there are no possible fits
-- for the hole in the environment, an alternative generator is used instead.
sampleDataConOr
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -- ^ The types in scope while generating
  -> Type
  -- ^ The hole to generate a fit for
  -> (Type -> CoreGenT m Term)
  -- ^ A generator for sub-holes (used when partially applying a fit)
  -> CoreGenT m Term
  -- ^ A generator to use if there are no hole fits
  -> CoreGenT m Term
sampleDataConOr tcm hole genSub genOr =
  sampleDataCon <|> genOr
 where
  sampleDataCon = do
    -- TODO We cannot fill in any datacon where the type is polymorphic. This
    -- is because sampleUniqMap will never pick one because it cannot see the
    -- fit is valid without unification. See the TODO by sampleUniqMap.
    --
    -- We mitigate this by not generating algebraic type constructors with
    -- kinds other than Type, i.e. no type params / poly kinds. See the
    -- 'genAlgTyCon' function and NOTE [finding more complex fits].
    let dcs = concatMap tyConDataCons tcm
    let dcm = listToUniqMap (zip dcs dcs)
    (dc, holes) <- sampleUniqMap (const True) hole dcm
    holeFills <- traverse genSub holes

    pure (mkTmApps (Data dc) holeFills)

-- | Attempt to sample an identifier which can be made to fit a hole of the
-- desired type. If this is not possible (due to nothing in the environment
-- matching) then the given alternative generator is used instead.
--
sampleIdOr
  :: forall m
   . (Alternative m, MonadGen m)
  => UniqMap (Either TyVar Id)
  -- ^ The currently bound type and term variables
  -> Type
  -- ^ The hole to generate a fit for
  -> (Type -> CoreGenT m Term)
  -- ^ A generator for sub-holes (used when partially applying a fit)
  -> CoreGenT m Term
  -- ^ A generator to use if there are no hole fits
  -> CoreGenT m Term
sampleIdOr env hole genSub genOr =
  sampleId <|> genOr
 where
  sampleId = do
    let tmEnv = mapMaybeUniqMap (either (const Nothing) Just) env
    (i, holes) <- sampleUniqMap (const True) hole tmEnv
    holeFills <- traverse genSub holes

    pure (mkTmApps (Var i) holeFills)

-- | Generate a term that is valid for the given type constructor map and
-- environment of free type and term variables. The term generated must have
-- the specified type.
--
genTermFrom
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -- ^ The types in scope while generating
  -> UniqMap (Either TyVar Id)
  -- ^ The currently bound type and term variables
  -> Type
  -- ^ The type of the term being generated
  -> CoreGenT m Term
genTermFrom tcm env hole =
  let genSub = genTermFrom tcm env
      genOr = genFreshTerm tcm env hole
   in Gen.choice
        [ sampleDataConOr tcm hole genSub genOr
        , sampleIdOr env hole genSub genOr
        ]

{-
NOTE [generated terms]
~~~~~~~~~~~~~~~~~~~~~~
Term generation is currently limited in some ways. This is for no particular
reason other than to make the generator easier to understand. For example

  * when the hole is a forall or a function, a lambda (or tick) is inserted
    instead of allowing expressions like letrec or case which could still have
    the desired type

  * primitives are not currently generated, as we likely want to build a
    collection of known primitives when we build the environments before making
    types and terms for tests

  * casts are currently not generated, as the majority are discarded by Clash
    during the GHC2Core stage. See PR #1064.
-}

-- | Generate a "fresh" term, i.e. one which is randomly created according to
-- the type of the hole, rather than sampling from the known variables or data
-- constructors.
--
-- This generator will fail if there are no values for the given hole.
--
genFreshTerm
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> UniqMap (Either TyVar Id)
  -> Type
  -> CoreGenT m Term
genFreshTerm tcm env hole =
  -- We need to normalize in case the hole is a type family.
  --
  -- TODO When casts are supported, this will need a cast between the
  -- normalized type and the given type.
  case normalizeType tcm hole of
    -- Hole: forall i. a
    normHole@(ForAllTy i a) ->
      Gen.recursive Gen.choice
        [TyLam i <$> genTermFrom tcm (extendUniqMap i (Left i) env) a]
        [Tick <$> genTickInfo tcm <*> genFreshTerm tcm env normHole]

    AnnType _ a ->
      genTermFrom tcm env a

    normHole ->
      case tyView normHole of
        -- Hole: a -> b
        FunTy a b ->
          Gen.recursive Gen.choice
            [do i <- genLocalId a (genFreshName (uniqMapToUniqSet env) genVarName)
                Gen.subterm (genTermFrom tcm (extendUniqMap i (Right i) env) b) (Lam i)
            ]
            [Tick <$> genTickInfo tcm <*> genFreshTerm tcm env normHole]

        -- Hole: Primitive type constructor.
        TyConApp tcn []
          |  Just PrimTyCon{} <- lookupUniqMap tcn tcm
          -> Gen.recursive Gen.choice
               [Literal <$> genLiteralFrom normHole]
               -- We may fail to generate a case expression if there is nothing
               -- in scope to use as a subject. If this happens, let bindings
               -- are introduced so next time genCase is called it does not fail.
               [ genCase tcm env normHole <|> genLetrec tcm env normHole
               , genLetrec tcm env normHole
               ]

        -- Hole: Algebraic type constructor.
        TyConApp tcn _
          |  Just AlgTyCon{} <- lookupUniqMap tcn tcm
          -- We may have got here by trying to fill the hole with an identifier, so
          -- it makes sense to try again. If we got here by sampleDataConOr, the
          -- data constructor is isomorphic to Void, and we will hit the error.
          -> Gen.recursive Gen.choice
               [sampleDataConOr tcm hole (genTermFrom tcm env)
                 (error ("No term level value for hole: " <> showPpr hole))]
               -- We may fail to generate a case expression if there is nothing
               -- in scope to use as a subject. If this happens, let bindings
               -- are introduced so next time genCase is called it does not fail.
               [ genCase tcm env normHole <|> genLetrec tcm env normHole
               , genLetrec tcm env normHole
               ]

        _ ->
          error ("No term level value for hole: " <> showPpr normHole)

-- TODO
-- genIsMultiPrim
-- genPrimInfo
-- genPrimUnfolding
-- genMultiPrimInfo
-- genWorkInfo

genLetrec
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> UniqMap (Either TyVar Id)
  -> Type
  -> CoreGenT m Term
genLetrec tcm env hole = do
  binds <- genLetBindings tcm env
  let vars = fmap fst binds
  let env' = extendListUniqMap env (zip vars (fmap Right vars))

  body <- genTermFrom tcm env' hole

  pure (Letrec binds body)

genLetBindings
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> UniqMap (Either TyVar Id)
  -> CoreGenT m [LetBinding]
genLetBindings tcm env = do
  let tyEnv = mapMaybeUniqMap (either Just (const Nothing)) env
  -- Limit the number of new bindings to 8 to prevent an explosion in the
  -- number of sub-holes to generate.
  types <- Gen.list (Range.linear 1 8) (genMonoTypeFrom tcm tyEnv liftedTypeKind)
  vars <- genVars genLocalId types genVarName

  forM (zip vars types) $ \(v, ty) ->
    -- Bindings can be indirectly recursive, but not directly recursive. This
    -- stops the generator from generating let x = x in ...
    let vars' = filter (/= v) vars
        env' = extendListUniqMap env (zip vars' (fmap Right vars'))
     in (v,) <$> genTermFrom tcm env' ty

{-
NOTE [generating useful case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When generating case expressions, it is perfectly valid to generate something
like the following:

  case C x_1 ... x_n of
    C p_1 ... p_n -> ...

However, we want to favour generating the more interesting case expressions
where the subject is an application on variables, like so

  case v x_1 ... x_n of
    C p_1 ... p_i -> ...
    D p_1 ... p_j -> ...
    E p_1 ... p_k -> ...

Likewise, we want to avoid generating case expressions with literals as the
subject, or function types (where the only viable pattern is DefaultPat). While
these are valid terms to generate, the generator will naturally bias towards
them, making too many of the tests too "artificial" in nature.

A downside of this approach is that generating a case expression is not
guaranteed to work, as if there is nothing in the environment that can be used
as the subject then the generator will return 'empty'. However, since letrec
always passes and introduces new bindings, we fallback to this if case fails.
This means the failure will happen at most once when generating.
-}

genCase
  :: forall m
   . (Alternative m, MonadGen m)
  => TyConMap
  -> UniqMap (Either TyVar Id)
  -> Type
  -> CoreGenT m Term
genCase tcm env altTy = do
  -- I need to select something as the subject. It can be any type, but should
  -- bias towards something from the environment where possible. It may not be
  -- possible though, so I should be able to fallback to just building a term.
  let tmEnv = mapMaybeUniqMap (either (const Nothing) Just) env
  subj <- sampleSubjFrom tmEnv
  let subjTy = inferCoreTypeOf tcm subj

  let maxPats = case fmap fst (splitTyConAppM subjTy) of
                  -- We can generate at most as many alternatives as there are
                  -- constructors in the type being matched.
                  Just tcn
                    |  Just tc <- lookupUniqMap tcn tcm
                    -> length (tyConDataCons tc)

                    -- This is probably a literal, set the upper bound to 8 to
                    -- prevent explosion in the number of sub-holes.
                    |  otherwise
                    -> 8

  -- The patterns are generated first as a Set. This prevents the generation of
  -- the same pattern multiple times, which we do not expect to happen.
  pats <- Gen.set (Range.linear 1 maxPats) (genPat tcm subjTy)
  alts <- traverse genAltFrom (Set.toList pats)

  pure (Case subj altTy alts)
 where
  -- Subjects are applications on variables in the environment.
  -- See NOTE [generating useful case expressions].
  sampleSubjFrom :: UniqMap Id -> CoreGenT m Term
  sampleSubjFrom tmEnv = do
    (v, holes) <- sampleAnyUniqMap tmEnv
    holeFills <- traverse (genTermFrom tcm env) holes

    pure (mkTmApps (Var v) holeFills)

  genAltFrom :: Pat -> CoreGenT m Alt
  genAltFrom pat = do
    let (tvs, ids) = patIds pat
    let toTvBind x = (varUniq x, Left x)
    let toIdBind x = (varUniq x, Right x)

    -- Generate the terms in alternatives with the newly bound vars in scope.
    let env' = extendListUniqMap env (fmap toTvBind tvs <> fmap toIdBind ids)
    term <- genTermFrom tcm env' altTy

    pure (pat, term)

genPat
  :: forall m
   . MonadGen m
  => TyConMap
  -> Type
  -> m Pat
genPat tcm hole =
  case tyView (normalizeType tcm hole) of
    TyConApp tcn _ ->
      case lookupUniqMap tcn tcm of
        Just PrimTyCon{} ->
          -- For Integer and Natural, real code could also have  DataPat, but
          -- we don't have the constructors in the TyConMap.
          Gen.choice
            [ Gen.constant DefaultPat
            , LitPat <$> genLiteralFrom (mkTyConTy tcn)
            ]

        Just tc@AlgTyCon{}  ->
          -- get the data constructors
          -- find the arguments
          -- generate ext tyvars and ids for arguments
          Gen.choice
            [ Gen.constant DefaultPat
            , genDataPat tc
            ]

        _ ->
          pure DefaultPat

    _ ->
      pure DefaultPat
 where
  genDataPat tc =
    let dcs = tyConDataCons tc
     in if null dcs
          then error ("No data constructors for hole: " <> showPpr hole)
          else do
            dc <- Gen.element dcs
            ids <- genVars genLocalId (dcArgTys dc) genVarName

            -- TODO Do I need to change the uniques here
            pure (DataPat dc (dcExtTyVars dc) ids)

-- TODO genCast

genTickInfo :: forall m. MonadGen m => TyConMap -> m TickInfo
genTickInfo tcm =
  Gen.choice
    [ NameMod <$> genNameMod <*> genClosedKindFrom tcm typeSymbolKind
    , Gen.constant DeDup
    , Gen.constant NoDeDup
    ]

genNameMod :: forall m. MonadGen m => m NameMod
genNameMod = Gen.element [PrefixName, SuffixName, SuffixNameP, SetName]
