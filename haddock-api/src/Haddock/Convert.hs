{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

{- |
Module      :  Haddock.Convert
Copyright   :  (c) Isaac Dupree 2009,
License     :  BSD-like

Maintainer  :  haddock@projects.haskell.org
Stability   :  experimental
Portability :  portable

Conversion between TyThing and HsDecl. This functionality may be moved into
GHC at some point.
-}
module Haddock.Convert
  ( tyThingToLHsDecl
  , PrintRuntimeReps (..)
  ) where

import Data.Either (lefts, rights)

import qualified GHC.Builtin.Names as CompatGHC
import qualified GHC.Builtin.Types as CompatGHC
import qualified GHC.Builtin.Types.Prim as CompatGHC
import qualified GHC.Core.Class as CompatGHC
import qualified GHC.Core.Coercion.Axiom as CompatGHC
import qualified GHC.Core.ConLike as CompatGHC
import qualified GHC.Core.DataCon as CompatGHC
import qualified GHC.Core.PatSyn as CompatGHC
import qualified GHC.Core.TyCo.Rep as CompatGHC
import qualified GHC.Core.TyCon as CompatGHC
import qualified GHC.Core.Type as CompatGHC
import qualified GHC.Data.Bag as CompatGHC
import qualified GHC.Hs as CompatGHC
import qualified GHC.Types.Basic as CompatGHC
import qualified GHC.Types.Fixity as CompatGHC
import qualified GHC.Types.Name as CompatGHC
import qualified GHC.Types.Name.Reader as CompatGHC
import qualified GHC.Types.Name.Set as CompatGHC
import qualified GHC.Types.SourceText as CompatGHC
import qualified GHC.Types.SrcLoc as CompatGHC
import qualified GHC.Types.TyThing as CompatGHC
import qualified GHC.Types.Var as CompatGHC
import qualified GHC.Types.Var.Set as CompatGHC
import qualified GHC.Utils.Misc as CompatGHC
import qualified GHC.Utils.Panic.Plain as CompatGHC

import Haddock.GhcUtils (defaultRuntimeRepVars, mkEmptySigType, orderedFVs)
import Haddock.Types (ErrMsg, errMsgUnlines)

import Data.Maybe (catMaybes, mapMaybe, maybeToList)

{- | Whether or not to default 'CompatGHC.RuntimeRep' variables to 'CompatGHC.LiftedRep'. Check
out Note [Defaulting RuntimeRep variables] in GHC.Iface.Type for the
motivation.
-}
data PrintRuntimeReps = ShowRuntimeRep | HideRuntimeRep

-- the main function here! yay!
tyThingToLHsDecl ::
  PrintRuntimeReps
  -> CompatGHC.TyThing
  -> Either ErrMsg ([ErrMsg], (CompatGHC.HsDecl CompatGHC.GhcRn))
tyThingToLHsDecl prr t = case t of
  -- ids (functions and zero-argument a.k.a. CAFs) get a type signature.
  -- Including built-in functions like seq.
  -- foreign-imported functions could be represented with ForD
  -- instead of SigD if we wanted...
  --
  -- in a future code version we could turn idVarDetails = foreign-call
  -- into a ForD instead of a SigD if we wanted.  Haddock doesn't
  -- need to care.
  CompatGHC.AnId i -> allOK $ CompatGHC.SigD CompatGHC.noExtField (synifyIdSig prr ImplicitizeForAll [] i)
  -- type-constructors (e.g. Maybe) are complicated, put the definition
  -- later in the file (also it's used for class associated-types too.)
  CompatGHC.ATyCon tc
    | Just cl <- CompatGHC.tyConClass_maybe tc -> -- classes are just a little tedious
        let extractFamilyDecl :: CompatGHC.TyClDecl a -> Either ErrMsg (CompatGHC.FamilyDecl a)
            extractFamilyDecl (CompatGHC.FamDecl _ d) = return d
            extractFamilyDecl _ =
              Left "tyThingToLHsDecl: impossible associated tycon"

            cvt :: CompatGHC.HsTyVarBndr flag CompatGHC.GhcRn -> CompatGHC.HsType CompatGHC.GhcRn
            -- Without this signature, we trigger GHC#18932
            cvt (CompatGHC.UserTyVar _ _ n) = CompatGHC.HsTyVar CompatGHC.noAnn CompatGHC.NotPromoted n
            cvt (CompatGHC.KindedTyVar _ _ (CompatGHC.L name_loc n) kind) =
              CompatGHC.HsKindSig
                CompatGHC.noAnn
                ( CompatGHC.L
                    (CompatGHC.na2la name_loc)
                    (CompatGHC.HsTyVar CompatGHC.noAnn CompatGHC.NotPromoted (CompatGHC.L name_loc n))
                )
                kind

            -- \| Convert a LHsTyVarBndr to an equivalent LHsType.
            hsLTyVarBndrToType ::
              CompatGHC.LHsTyVarBndr flag CompatGHC.GhcRn -> CompatGHC.LHsType CompatGHC.GhcRn
            hsLTyVarBndrToType = CompatGHC.mapLoc cvt

            extractFamDefDecl ::
              CompatGHC.FamilyDecl CompatGHC.GhcRn -> CompatGHC.Type -> CompatGHC.TyFamDefltDecl CompatGHC.GhcRn
            extractFamDefDecl fd rhs =
              CompatGHC.TyFamInstDecl CompatGHC.noAnn $
                CompatGHC.FamEqn
                  { CompatGHC.feqn_ext = CompatGHC.noAnn
                  , CompatGHC.feqn_tycon = CompatGHC.fdLName fd
                  , CompatGHC.feqn_bndrs =
                      CompatGHC.HsOuterImplicit{CompatGHC.hso_ximplicit = CompatGHC.hsq_ext (CompatGHC.fdTyVars fd)}
                  , CompatGHC.feqn_pats =
                      map (CompatGHC.HsValArg . hsLTyVarBndrToType) $
                        CompatGHC.hsq_explicit $
                          CompatGHC.fdTyVars fd
                  , CompatGHC.feqn_fixity = CompatGHC.fdFixity fd
                  , CompatGHC.feqn_rhs = synifyType WithinType [] rhs
                  }

            extractAtItem ::
              CompatGHC.ClassATItem
              -> Either
                  ErrMsg
                  (CompatGHC.LFamilyDecl CompatGHC.GhcRn, Maybe (CompatGHC.LTyFamDefltDecl CompatGHC.GhcRn))
            extractAtItem (CompatGHC.ATI at_tc def) = do
              tyDecl <- synifyTyCon prr Nothing at_tc
              famDecl <- extractFamilyDecl tyDecl
              let defEqnTy = fmap (CompatGHC.noLocA . extractFamDefDecl famDecl . fst) def
              pure (CompatGHC.noLocA famDecl, defEqnTy)

            atTyClDecls = map extractAtItem (CompatGHC.classATItems cl)
            (atFamDecls, atDefFamDecls) = unzip (rights atTyClDecls)
            vs = CompatGHC.tyConVisibleTyVars (CompatGHC.classTyCon cl)
         in withErrs (lefts atTyClDecls) . CompatGHC.TyClD CompatGHC.noExtField $
              CompatGHC.ClassDecl
                { CompatGHC.tcdCtxt = Just $ synifyCtx (CompatGHC.classSCTheta cl)
                , CompatGHC.tcdLName = synifyNameN cl
                , CompatGHC.tcdTyVars = synifyTyVars vs
                , CompatGHC.tcdFixity = synifyFixity cl
                , CompatGHC.tcdFDs =
                    map
                      ( \(l, r) ->
                          CompatGHC.noLocA
                            ( CompatGHC.FunDep
                                CompatGHC.noAnn
                                (map (CompatGHC.noLocA . CompatGHC.getName) l)
                                (map (CompatGHC.noLocA . CompatGHC.getName) r)
                            )
                      )
                      $ snd
                      $ CompatGHC.classTvsFds cl
                , CompatGHC.tcdSigs =
                    CompatGHC.noLocA
                      ( CompatGHC.MinimalSig CompatGHC.noAnn CompatGHC.NoSourceText
                          . CompatGHC.noLocA
                          . fmap CompatGHC.noLocA $
                          CompatGHC.classMinimalDef cl
                      )
                      : [ CompatGHC.noLocA tcdSig
                        | clsOp <- CompatGHC.classOpItems cl
                        , tcdSig <- synifyTcIdSig vs clsOp
                        ]
                , CompatGHC.tcdMeths = CompatGHC.emptyBag -- ignore default method definitions, they don't affect signature
                -- class associated-types are a subset of TyCon:
                , CompatGHC.tcdATs = atFamDecls
                , CompatGHC.tcdATDefs = catMaybes atDefFamDecls
                , CompatGHC.tcdDocs = [] -- we don't have any docs at this point
                , CompatGHC.tcdCExt = CompatGHC.emptyNameSet
                }
    | otherwise ->
        synifyTyCon prr Nothing tc >>= allOK . CompatGHC.TyClD CompatGHC.noExtField
  -- type-constructors (e.g. Maybe) are complicated, put the definition
  -- later in the file (also it's used for class associated-types too.)
  CompatGHC.ACoAxiom ax -> synifyAxiom ax >>= allOK
  -- a data-constructor alone just gets rendered as a function:
  CompatGHC.AConLike (CompatGHC.RealDataCon dc) ->
    allOK $
      CompatGHC.SigD
        CompatGHC.noExtField
        ( CompatGHC.TypeSig
            CompatGHC.noAnn
            [synifyNameN dc]
            (synifySigWcType ImplicitizeForAll [] (CompatGHC.dataConWrapperType dc))
        )
  CompatGHC.AConLike (CompatGHC.PatSynCon ps) ->
    allOK . CompatGHC.SigD CompatGHC.noExtField $
      CompatGHC.PatSynSig CompatGHC.noAnn [synifyNameN ps] (synifyPatSynSigType ps)
 where
  withErrs e x = return (e, x)
  allOK x = return (mempty, x)

synifyAxBranch :: CompatGHC.TyCon -> CompatGHC.CoAxBranch -> CompatGHC.TyFamInstEqn CompatGHC.GhcRn
synifyAxBranch tc (CompatGHC.CoAxBranch{CompatGHC.cab_tvs = tkvs, CompatGHC.cab_lhs = args, CompatGHC.cab_rhs = rhs}) =
  let name = synifyNameN tc
      args_types_only = CompatGHC.filterOutInvisibleTypes tc args
      typats = map (synifyType WithinType []) args_types_only
      annot_typats = zipWith3 annotHsType args_poly args_types_only typats
      hs_rhs = synifyType WithinType [] rhs
      outer_bndrs = CompatGHC.HsOuterImplicit{CompatGHC.hso_ximplicit = map CompatGHC.tyVarName tkvs}
   in -- TODO: this must change eventually
      CompatGHC.FamEqn
        { CompatGHC.feqn_ext = CompatGHC.noAnn
        , CompatGHC.feqn_tycon = name
        , CompatGHC.feqn_bndrs = outer_bndrs
        , CompatGHC.feqn_pats = map CompatGHC.HsValArg annot_typats
        , CompatGHC.feqn_fixity = synifyFixity name
        , CompatGHC.feqn_rhs = hs_rhs
        }
 where
  args_poly = tyConArgsPolyKinded tc

synifyAxiom :: CompatGHC.CoAxiom br -> Either ErrMsg (CompatGHC.HsDecl CompatGHC.GhcRn)
synifyAxiom ax@(CompatGHC.CoAxiom{CompatGHC.co_ax_tc = tc})
  | CompatGHC.isOpenTypeFamilyTyCon tc
  , Just branch <- CompatGHC.coAxiomSingleBranch_maybe ax =
      return $
        CompatGHC.InstD CompatGHC.noExtField $
          CompatGHC.TyFamInstD CompatGHC.noExtField $
            CompatGHC.TyFamInstDecl
              { CompatGHC.tfid_xtn = CompatGHC.noAnn
              , CompatGHC.tfid_eqn = synifyAxBranch tc branch
              }
  | Just ax' <- CompatGHC.isClosedSynFamilyTyConWithAxiom_maybe tc
  , CompatGHC.getUnique ax' == CompatGHC.getUnique ax -- without the getUniques, type error
    =
      synifyTyCon ShowRuntimeRep (Just ax) tc >>= return . CompatGHC.TyClD CompatGHC.noExtField
  | otherwise =
      Left "synifyAxiom: closed/open family confusion"

-- | Turn type constructors into data declarations, type families, or type synonyms
synifyTyCon ::
  PrintRuntimeReps
  -> Maybe (CompatGHC.CoAxiom br)
  -- ^ RHS of type synonym
  -> CompatGHC.TyCon
  -- ^ type constructor to convert
  -> Either ErrMsg (CompatGHC.TyClDecl CompatGHC.GhcRn)
synifyTyCon prr _coax tc
  | CompatGHC.isFunTyCon tc || CompatGHC.isPrimTyCon tc =
      return $
        CompatGHC.DataDecl
          { CompatGHC.tcdLName = synifyNameN tc
          , CompatGHC.tcdTyVars =
              CompatGHC.HsQTvs
                { CompatGHC.hsq_ext = [] -- No kind polymorphism
                , CompatGHC.hsq_explicit =
                    zipWith
                      mk_hs_tv
                      (map CompatGHC.scaledThing tyVarKinds)
                      CompatGHC.alphaTyVars -- a, b, c... which are unfortunately all kind *
                }
          , CompatGHC.tcdFixity = synifyFixity tc
          , CompatGHC.tcdDataDefn =
              CompatGHC.HsDataDefn
                { CompatGHC.dd_ext = CompatGHC.noExtField
                , CompatGHC.dd_ND = CompatGHC.DataType -- arbitrary lie, they are neither
                -- algebraic data nor newtype:
                , CompatGHC.dd_ctxt = Nothing
                , CompatGHC.dd_cType = Nothing
                , CompatGHC.dd_kindSig = synifyDataTyConReturnKind tc
                , -- we have their kind accurately:
                  CompatGHC.dd_cons = [] -- No constructors
                , CompatGHC.dd_derivs = []
                }
          , CompatGHC.tcdDExt = CompatGHC.DataDeclRn False CompatGHC.emptyNameSet
          }
 where
  -- tyConTyVars doesn't work on fun/prim, but we can make them up:
  mk_hs_tv realKind fakeTyVar
    | CompatGHC.isLiftedTypeKind realKind =
        CompatGHC.noLocA $
          CompatGHC.UserTyVar CompatGHC.noAnn () (CompatGHC.noLocA (CompatGHC.getName fakeTyVar))
    | otherwise =
        CompatGHC.noLocA $
          CompatGHC.KindedTyVar
            CompatGHC.noAnn
            ()
            (CompatGHC.noLocA (CompatGHC.getName fakeTyVar))
            (synifyKindSig realKind)

  conKind = defaultType prr (CompatGHC.tyConKind tc)
  tyVarKinds = fst . CompatGHC.splitFunTys . snd . CompatGHC.splitInvisPiTys $ conKind
synifyTyCon _prr _coax tc
  | Just flav <- CompatGHC.famTyConFlav_maybe tc =
      case flav of
        -- Type families
        CompatGHC.OpenSynFamilyTyCon -> mkFamDecl CompatGHC.OpenTypeFamily
        CompatGHC.ClosedSynFamilyTyCon mb
          | Just (CompatGHC.CoAxiom{CompatGHC.co_ax_branches = branches}) <- mb ->
              mkFamDecl $
                CompatGHC.ClosedTypeFamily $
                  Just $
                    map (CompatGHC.noLocA . synifyAxBranch tc) (CompatGHC.fromBranches branches)
          | otherwise ->
              mkFamDecl $ CompatGHC.ClosedTypeFamily $ Just []
        CompatGHC.BuiltInSynFamTyCon{} ->
          mkFamDecl $ CompatGHC.ClosedTypeFamily $ Just []
        CompatGHC.AbstractClosedSynFamilyTyCon{} ->
          mkFamDecl $ CompatGHC.ClosedTypeFamily Nothing
        CompatGHC.DataFamilyTyCon{} ->
          mkFamDecl CompatGHC.DataFamily
 where
  resultVar = CompatGHC.famTcResVar tc
  mkFamDecl i =
    return $
      CompatGHC.FamDecl CompatGHC.noExtField $
        CompatGHC.FamilyDecl
          { CompatGHC.fdExt = CompatGHC.noAnn
          , CompatGHC.fdInfo = i
          , CompatGHC.fdTopLevel = CompatGHC.TopLevel
          , CompatGHC.fdLName = synifyNameN tc
          , CompatGHC.fdTyVars = synifyTyVars (CompatGHC.tyConVisibleTyVars tc)
          , CompatGHC.fdFixity = synifyFixity tc
          , CompatGHC.fdResultSig =
              synifyFamilyResultSig resultVar (CompatGHC.tyConResKind tc)
          , CompatGHC.fdInjectivityAnn =
              synifyInjectivityAnn
                resultVar
                (CompatGHC.tyConTyVars tc)
                (CompatGHC.tyConInjectivityInfo tc)
          }
synifyTyCon _prr coax tc
  | Just ty <- CompatGHC.synTyConRhs_maybe tc =
      return $
        CompatGHC.SynDecl
          { CompatGHC.tcdSExt = CompatGHC.emptyNameSet
          , CompatGHC.tcdLName = synifyNameN tc
          , CompatGHC.tcdTyVars = synifyTyVars (CompatGHC.tyConVisibleTyVars tc)
          , CompatGHC.tcdFixity = synifyFixity tc
          , CompatGHC.tcdRhs = synifyType WithinType [] ty
          }
  | otherwise =
      -- (closed) newtype and data
      let
        alg_nd = if CompatGHC.isNewTyCon tc then CompatGHC.NewType else CompatGHC.DataType
        alg_ctx = synifyCtx (CompatGHC.tyConStupidTheta tc)
        name = case coax of
          Just a -> synifyNameN a -- Data families are named according to their
          -- CoAxioms, not their TyCons
          _ -> synifyNameN tc
        tyvars = synifyTyVars (CompatGHC.tyConVisibleTyVars tc)
        kindSig = synifyDataTyConReturnKind tc
        -- The data constructors.
        --
        -- Any data-constructors not exported from the module that *defines* the
        -- type will not (cannot) be included.
        --
        -- Very simple constructors, Haskell98 with no existentials or anything,
        -- probably look nicer in non-GADT syntax.  In source code, all constructors
        -- must be declared with the same (GADT vs. not) syntax, and it probably
        -- is less confusing to follow that principle for the documentation as well.
        --
        -- There is no sensible infix-representation for GADT-syntax constructor
        -- declarations.  They cannot be made in source code, but we could end up
        -- with some here in the case where some constructors use existentials.
        -- That seems like an acceptable compromise (they'll just be documented
        -- in prefix position), since, otherwise, the logic (at best) gets much more
        -- complicated. (would use dataConIsInfix.)
        use_gadt_syntax = CompatGHC.isGadtSyntaxTyCon tc
        consRaw = map (synifyDataCon use_gadt_syntax) (CompatGHC.tyConDataCons tc)
        cons = rights consRaw
        -- "deriving" doesn't affect the signature, no need to specify any.
        alg_deriv = []
        defn =
          CompatGHC.HsDataDefn
            { CompatGHC.dd_ext = CompatGHC.noExtField
            , CompatGHC.dd_ND = alg_nd
            , CompatGHC.dd_ctxt = Just alg_ctx
            , CompatGHC.dd_cType = Nothing
            , CompatGHC.dd_kindSig = kindSig
            , CompatGHC.dd_cons = cons
            , CompatGHC.dd_derivs = alg_deriv
            }
       in
        case lefts consRaw of
          [] ->
            return $
              CompatGHC.DataDecl
                { CompatGHC.tcdLName = name
                , CompatGHC.tcdTyVars = tyvars
                , CompatGHC.tcdFixity = synifyFixity name
                , CompatGHC.tcdDataDefn = defn
                , CompatGHC.tcdDExt = CompatGHC.DataDeclRn False CompatGHC.emptyNameSet
                }
          dataConErrs -> Left $ errMsgUnlines dataConErrs

{- | In this module, every TyCon being considered has come from an interface
file. This means that when considering a data type constructor such as:

> data Foo (w :: *) (m :: * -> *) (a :: *)

Then its tyConKind will be (* -> (* -> *) -> * -> *). But beware! We are
also rendering the type variables of Foo, so if we synify the tyConKind of
Foo in full, we will end up displaying this in Haddock:

> data Foo (w :: *) (m :: * -> *) (a :: *)
>   :: * -> (* -> *) -> * -> *

Which is entirely wrong (#548). We only want to display the /return/ kind,
which this function obtains.
-}
synifyDataTyConReturnKind :: CompatGHC.TyCon -> Maybe (CompatGHC.LHsKind CompatGHC.GhcRn)
synifyDataTyConReturnKind tc
  | CompatGHC.isLiftedTypeKind ret_kind = Nothing -- Don't bother displaying :: *
  | otherwise = Just (synifyKindSig ret_kind)
 where
  ret_kind = CompatGHC.tyConResKind tc

synifyInjectivityAnn ::
  Maybe CompatGHC.Name
  -> [CompatGHC.TyVar]
  -> CompatGHC.Injectivity
  -> Maybe (CompatGHC.LInjectivityAnn CompatGHC.GhcRn)
synifyInjectivityAnn Nothing _ _ = Nothing
synifyInjectivityAnn _ _ CompatGHC.NotInjective = Nothing
synifyInjectivityAnn (Just lhs) tvs (CompatGHC.Injective inj) =
  let rhs = map (CompatGHC.noLocA . CompatGHC.tyVarName) (CompatGHC.filterByList inj tvs)
   in Just $ CompatGHC.noLocA $ CompatGHC.InjectivityAnn CompatGHC.noAnn (CompatGHC.noLocA lhs) rhs

synifyFamilyResultSig ::
  Maybe CompatGHC.Name -> CompatGHC.Kind -> CompatGHC.LFamilyResultSig CompatGHC.GhcRn
synifyFamilyResultSig Nothing kind
  | CompatGHC.isLiftedTypeKind kind = CompatGHC.noLocA $ CompatGHC.NoSig CompatGHC.noExtField
  | otherwise = CompatGHC.noLocA $ CompatGHC.KindSig CompatGHC.noExtField (synifyKindSig kind)
synifyFamilyResultSig (Just name) kind =
  CompatGHC.noLocA $
    CompatGHC.TyVarSig
      CompatGHC.noExtField
      ( CompatGHC.noLocA $
          CompatGHC.KindedTyVar CompatGHC.noAnn () (CompatGHC.noLocA name) (synifyKindSig kind)
      )

-- User beware: it is your responsibility to pass True (use_gadt_syntax)
-- for any constructor that would be misrepresented by omitting its
-- result-type.
-- But you might want pass False in simple enough cases,
-- if you think it looks better.
synifyDataCon :: Bool -> CompatGHC.DataCon -> Either ErrMsg (CompatGHC.LConDecl CompatGHC.GhcRn)
synifyDataCon use_gadt_syntax dc =
  let
    -- dataConIsInfix allegedly tells us whether it was declared with
    -- infix *syntax*.
    use_infix_syntax = CompatGHC.dataConIsInfix dc
    use_named_field_syntax = not (null field_tys)
    name = synifyNameN dc
    -- con_qvars means a different thing depending on gadt-syntax
    (_univ_tvs, ex_tvs, _eq_spec, theta, arg_tys, res_ty) = CompatGHC.dataConFullSig dc
    user_tvbndrs = CompatGHC.dataConUserTyVarBinders dc -- Used for GADT data constructors
    outer_bndrs
      | null user_tvbndrs =
          CompatGHC.HsOuterImplicit{CompatGHC.hso_ximplicit = []}
      | otherwise =
          CompatGHC.HsOuterExplicit
            { CompatGHC.hso_xexplicit = CompatGHC.noExtField
            , CompatGHC.hso_bndrs = map synifyTyVarBndr user_tvbndrs
            }

    -- skip any EqTheta, use 'orig'inal syntax
    ctx
      | null theta = Nothing
      | otherwise = Just $ synifyCtx theta

    linear_tys =
      zipWith
        ( \ty bang ->
            let tySyn = synifyType WithinType [] (CompatGHC.scaledThing ty)
             in case bang of
                  (CompatGHC.HsSrcBang _ CompatGHC.NoSrcUnpack CompatGHC.NoSrcStrict) -> tySyn
                  bang' -> CompatGHC.noLocA $ CompatGHC.HsBangTy CompatGHC.noAnn bang' tySyn
        )
        arg_tys
        (CompatGHC.dataConSrcBangs dc)

    field_tys = zipWith con_decl_field (CompatGHC.dataConFieldLabels dc) linear_tys
    con_decl_field fl synTy =
      CompatGHC.noLocA $
        CompatGHC.ConDeclField
          CompatGHC.noAnn
          [ CompatGHC.noLocA $
              CompatGHC.FieldOcc
                (CompatGHC.flSelector fl)
                (CompatGHC.noLocA $ CompatGHC.mkVarUnqual $ CompatGHC.flLabel fl)
          ]
          synTy
          Nothing

    mk_h98_arg_tys :: Either ErrMsg (CompatGHC.HsConDeclH98Details CompatGHC.GhcRn)
    mk_h98_arg_tys = case (use_named_field_syntax, use_infix_syntax) of
      (True, True) -> Left "synifyDataCon: contradiction!"
      (True, False) -> return $ CompatGHC.RecCon (CompatGHC.noLocA field_tys)
      (False, False) -> return $ CompatGHC.PrefixCon CompatGHC.noTypeArgs (map CompatGHC.hsUnrestricted linear_tys)
      (False, True) -> case linear_tys of
        [a, b] -> return $ CompatGHC.InfixCon (CompatGHC.hsUnrestricted a) (CompatGHC.hsUnrestricted b)
        _ -> Left "synifyDataCon: infix with non-2 args?"

    mk_gadt_arg_tys :: CompatGHC.HsConDeclGADTDetails CompatGHC.GhcRn
    mk_gadt_arg_tys
      | use_named_field_syntax = CompatGHC.RecConGADT (CompatGHC.noLocA field_tys) CompatGHC.noHsUniTok
      | otherwise = CompatGHC.PrefixConGADT (map CompatGHC.hsUnrestricted linear_tys)
   in
    -- finally we get synifyDataCon's result!
    if use_gadt_syntax
      then do
        let hat = mk_gadt_arg_tys
        return $
          CompatGHC.noLocA $
            CompatGHC.ConDeclGADT
              { CompatGHC.con_g_ext = CompatGHC.noAnn
              , CompatGHC.con_names = [name]
              , CompatGHC.con_bndrs = CompatGHC.noLocA outer_bndrs
              , CompatGHC.con_mb_cxt = ctx
              , CompatGHC.con_g_args = hat
              , CompatGHC.con_res_ty = synifyType WithinType [] res_ty
              , CompatGHC.con_doc = Nothing
              }
      else do
        hat <- mk_h98_arg_tys
        return $
          CompatGHC.noLocA $
            CompatGHC.ConDeclH98
              { CompatGHC.con_ext = CompatGHC.noAnn
              , CompatGHC.con_name = name
              , CompatGHC.con_forall = False
              , CompatGHC.con_ex_tvs =
                  map (synifyTyVarBndr . (CompatGHC.mkTyCoVarBinder CompatGHC.InferredSpec)) ex_tvs
              , CompatGHC.con_mb_cxt = ctx
              , CompatGHC.con_args = hat
              , CompatGHC.con_doc = Nothing
              }

synifyNameN :: (CompatGHC.NamedThing n) => n -> CompatGHC.LocatedN CompatGHC.Name
synifyNameN n =
  CompatGHC.L
    (CompatGHC.noAnnSrcSpan $ CompatGHC.srcLocSpan (CompatGHC.getSrcLoc n))
    (CompatGHC.getName n)

-- synifyName :: NamedThing n => n -> LocatedA Name
-- synifyName n = L (noAnnSrcSpan $ srcLocSpan (getSrcLoc n)) (getName n)

{- | Guess the fixity of a something with a name. This isn't quite right, since
a user can always declare an infix name in prefix form or a prefix name in
infix form. Unfortunately, that is not something we can usually reconstruct.
-}
synifyFixity :: (CompatGHC.NamedThing n) => n -> CompatGHC.LexicalFixity
synifyFixity n
  | CompatGHC.isSymOcc (CompatGHC.getOccName n) = CompatGHC.Infix
  | otherwise = CompatGHC.Prefix

synifyIdSig ::
  PrintRuntimeReps
  -- ^ are we printing tyvars of kind 'CompatGHC.RuntimeRep'?
  -> SynifyTypeState
  -- ^ what to do with a 'forall'
  -> [CompatGHC.TyVar]
  -- ^ free variables in the type to convert
  -> CompatGHC.Id
  -- ^ the 'CompatGHC.Id' from which to get the type signature
  -> CompatGHC.Sig CompatGHC.GhcRn
synifyIdSig prr s vs i = CompatGHC.TypeSig CompatGHC.noAnn [synifyNameN i] (synifySigWcType s vs t)
 where
  t = defaultType prr (CompatGHC.varType i)

{- | Turn a 'CompatGHC.ClassOpItem' into a list of signatures. The list returned is going
to contain the synified 'CompatGHC.ClassOpSig' as well (when appropriate) a default
'CompatGHC.ClassOpSig'.
-}
synifyTcIdSig :: [CompatGHC.TyVar] -> CompatGHC.ClassOpItem -> [CompatGHC.Sig CompatGHC.GhcRn]
synifyTcIdSig vs (i, dm) =
  [CompatGHC.ClassOpSig CompatGHC.noAnn False [synifyNameN i] (mainSig (CompatGHC.varType i))]
    ++ [ CompatGHC.ClassOpSig CompatGHC.noAnn True [CompatGHC.noLocA dn] (defSig dt)
       | Just (dn, CompatGHC.GenericDM dt) <- [dm]
       ]
 where
  mainSig t = synifySigType DeleteTopLevelQuantification vs t
  defSig t = synifySigType ImplicitizeForAll vs t

synifyCtx :: [CompatGHC.PredType] -> CompatGHC.LHsContext CompatGHC.GhcRn
synifyCtx ts = CompatGHC.noLocA (map (synifyType WithinType []) ts)

synifyTyVars :: [CompatGHC.TyVar] -> CompatGHC.LHsQTyVars CompatGHC.GhcRn
synifyTyVars ktvs =
  CompatGHC.HsQTvs
    { CompatGHC.hsq_ext = []
    , CompatGHC.hsq_explicit = map synifyTyVar ktvs
    }

synifyTyVar :: CompatGHC.TyVar -> CompatGHC.LHsTyVarBndr () CompatGHC.GhcRn
synifyTyVar = synify_ty_var CompatGHC.emptyVarSet ()

synifyTyVarBndr ::
  CompatGHC.VarBndr CompatGHC.TyVar flag -> CompatGHC.LHsTyVarBndr flag CompatGHC.GhcRn
synifyTyVarBndr = synifyTyVarBndr' CompatGHC.emptyVarSet

synifyTyVarBndr' ::
  CompatGHC.VarSet
  -> CompatGHC.VarBndr CompatGHC.TyVar flag
  -> CompatGHC.LHsTyVarBndr flag CompatGHC.GhcRn
synifyTyVarBndr' no_kinds (CompatGHC.Bndr tv spec) = synify_ty_var no_kinds spec tv

{- | Like 'synifyTyVarBndr', but accepts a set of variables for which to omit kind
signatures (even if they don't have the lifted type kind).
-}
synify_ty_var ::
  CompatGHC.VarSet -> flag -> CompatGHC.TyVar -> CompatGHC.LHsTyVarBndr flag CompatGHC.GhcRn
synify_ty_var no_kinds flag tv
  | CompatGHC.isLiftedTypeKind kind || tv `CompatGHC.elemVarSet` no_kinds =
      CompatGHC.noLocA (CompatGHC.UserTyVar CompatGHC.noAnn flag (CompatGHC.noLocA name))
  | otherwise =
      CompatGHC.noLocA
        (CompatGHC.KindedTyVar CompatGHC.noAnn flag (CompatGHC.noLocA name) (synifyKindSig kind))
 where
  kind = CompatGHC.tyVarKind tv
  name = CompatGHC.getName tv

{- | Annotate (with HsKingSig) a type if the first parameter is True
and if the type contains a free variable.
This is used to synify type patterns for poly-kinded tyvars in
synifying class and type instances.
-}
annotHsType ::
  Bool -- True <=> annotate
  -> CompatGHC.Type
  -> CompatGHC.LHsType CompatGHC.GhcRn
  -> CompatGHC.LHsType CompatGHC.GhcRn
-- tiny optimization: if the type is annotated, don't annotate again.
annotHsType _ _ hs_ty@(CompatGHC.L _ (CompatGHC.HsKindSig{})) = hs_ty
annotHsType True ty hs_ty
  | not $
      CompatGHC.isEmptyVarSet $
        CompatGHC.filterVarSet CompatGHC.isTyVar $
          CompatGHC.tyCoVarsOfType ty =
      let ki = CompatGHC.typeKind ty
          hs_ki = synifyType WithinType [] ki
       in CompatGHC.noLocA (CompatGHC.HsKindSig CompatGHC.noAnn hs_ty hs_ki)
annotHsType _ _ hs_ty = hs_ty

{- | For every argument type that a type constructor accepts,
report whether or not the argument is poly-kinded. This is used to
eventually feed into 'annotThType'.
-}
tyConArgsPolyKinded :: CompatGHC.TyCon -> [Bool]
tyConArgsPolyKinded tc =
  map (is_poly_ty . CompatGHC.tyVarKind) tc_vis_tvs
    ++ map (is_poly_ty . CompatGHC.tyCoBinderType) tc_res_kind_vis_bndrs
    ++ repeat True
 where
  is_poly_ty :: CompatGHC.Type -> Bool
  is_poly_ty ty =
    not $
      CompatGHC.isEmptyVarSet $
        CompatGHC.filterVarSet CompatGHC.isTyVar $
          CompatGHC.tyCoVarsOfType ty

  tc_vis_tvs :: [CompatGHC.TyVar]
  tc_vis_tvs = CompatGHC.tyConVisibleTyVars tc

  tc_res_kind_vis_bndrs :: [CompatGHC.TyCoBinder]
  tc_res_kind_vis_bndrs = filter CompatGHC.isVisibleBinder $ fst $ CompatGHC.splitPiTys $ CompatGHC.tyConResKind tc

-- states of what to do with foralls:
data SynifyTypeState
  = -- | normal situation.  This is the safe one to use if you don't
    -- quite understand what's going on.
    WithinType
  | -- | beginning of a function definition, in which, to make it look
    --   less ugly, those rank-1 foralls (without kind annotations) are made
    --   implicit.
    ImplicitizeForAll
  | -- | because in class methods the context is added to the type
    --   (e.g. adding @forall a. Num a =>@ to @(+) :: a -> a -> a@)
    --   which is rather sensible,
    --   but we want to restore things to the source-syntax situation where
    --   the defining class gets to quantify all its functions for free!
    DeleteTopLevelQuantification

synifySigType ::
  SynifyTypeState -> [CompatGHC.TyVar] -> CompatGHC.Type -> CompatGHC.LHsSigType CompatGHC.GhcRn
-- The use of mkEmptySigType (which uses empty binders in OuterImplicit)
-- is a bit suspicious; what if the type has free variables?
synifySigType s vs ty = mkEmptySigType (synifyType s vs ty)

synifySigWcType ::
  SynifyTypeState -> [CompatGHC.TyVar] -> CompatGHC.Type -> CompatGHC.LHsSigWcType CompatGHC.GhcRn
-- Ditto (see synifySigType)
synifySigWcType s vs ty = CompatGHC.mkEmptyWildCardBndrs (mkEmptySigType (synifyType s vs ty))

synifyPatSynSigType :: CompatGHC.PatSyn -> CompatGHC.LHsSigType CompatGHC.GhcRn
-- Ditto (see synifySigType)
synifyPatSynSigType ps = mkEmptySigType (synifyPatSynType ps)

{- | Depending on the first argument, try to default all type variables of kind
'CompatGHC.RuntimeRep' to 'LiftedType'.
-}
defaultType :: PrintRuntimeReps -> CompatGHC.Type -> CompatGHC.Type
defaultType ShowRuntimeRep = id
defaultType HideRuntimeRep = defaultRuntimeRepVars

-- | Convert a core type into an 'CompatGHC.HsType'.
synifyType ::
  SynifyTypeState
  -- ^ what to do with a 'forall'
  -> [CompatGHC.TyVar]
  -- ^ free variables in the type to convert
  -> CompatGHC.Type
  -- ^ the type to convert
  -> CompatGHC.LHsType CompatGHC.GhcRn
synifyType _ _ (CompatGHC.TyVarTy tv) =
  CompatGHC.noLocA $
    CompatGHC.HsTyVar CompatGHC.noAnn CompatGHC.NotPromoted $
      CompatGHC.noLocA (CompatGHC.getName tv)
synifyType _ vs (CompatGHC.TyConApp tc tys) =
  maybe_sig res_ty
 where
  res_ty :: CompatGHC.LHsType CompatGHC.GhcRn
  res_ty
    -- Use */# instead of TYPE 'Lifted/TYPE 'Unlifted (#473)
    | tc `CompatGHC.hasKey` CompatGHC.tYPETyConKey
    , [CompatGHC.TyConApp rep [CompatGHC.TyConApp lev []]] <- tys
    , rep `CompatGHC.hasKey` CompatGHC.boxedRepDataConKey
    , lev `CompatGHC.hasKey` CompatGHC.liftedDataConKey =
        CompatGHC.noLocA
          ( CompatGHC.HsTyVar
              CompatGHC.noAnn
              CompatGHC.NotPromoted
              (CompatGHC.noLocA CompatGHC.liftedTypeKindTyConName)
          )
    -- Use non-prefix tuple syntax where possible, because it looks nicer.
    | Just sort <- CompatGHC.tyConTuple_maybe tc
    , CompatGHC.tyConArity tc == tys_len =
        CompatGHC.noLocA $
          CompatGHC.HsTupleTy
            CompatGHC.noAnn
            ( case sort of
                CompatGHC.BoxedTuple -> CompatGHC.HsBoxedOrConstraintTuple
                CompatGHC.ConstraintTuple -> CompatGHC.HsBoxedOrConstraintTuple
                CompatGHC.UnboxedTuple -> CompatGHC.HsUnboxedTuple
            )
            (map (synifyType WithinType vs) vis_tys)
    | CompatGHC.isUnboxedSumTyCon tc =
        CompatGHC.noLocA $ CompatGHC.HsSumTy CompatGHC.noAnn (map (synifyType WithinType vs) vis_tys)
    | Just dc <- CompatGHC.isPromotedDataCon_maybe tc
    , CompatGHC.isTupleDataCon dc
    , CompatGHC.dataConSourceArity dc == length vis_tys =
        CompatGHC.noLocA $
          CompatGHC.HsExplicitTupleTy CompatGHC.noExtField (map (synifyType WithinType vs) vis_tys)
    -- ditto for lists
    | CompatGHC.getName tc == CompatGHC.listTyConName
    , [ty] <- vis_tys =
        CompatGHC.noLocA $ CompatGHC.HsListTy CompatGHC.noAnn (synifyType WithinType vs ty)
    | tc == CompatGHC.promotedNilDataCon
    , [] <- vis_tys =
        CompatGHC.noLocA $ CompatGHC.HsExplicitListTy CompatGHC.noExtField CompatGHC.IsPromoted []
    | tc == CompatGHC.promotedConsDataCon
    , [ty1, ty2] <- vis_tys =
        let hTy = synifyType WithinType vs ty1
         in case synifyType WithinType vs ty2 of
              tTy
                | CompatGHC.L _ (CompatGHC.HsExplicitListTy _ CompatGHC.IsPromoted tTy') <- stripKindSig tTy ->
                    CompatGHC.noLocA $ CompatGHC.HsExplicitListTy CompatGHC.noExtField CompatGHC.IsPromoted (hTy : tTy')
                | otherwise ->
                    CompatGHC.noLocA $
                      CompatGHC.HsOpTy
                        CompatGHC.noAnn
                        CompatGHC.IsPromoted
                        hTy
                        (CompatGHC.noLocA $ CompatGHC.getName tc)
                        tTy
    -- ditto for implicit parameter tycons
    | tc `CompatGHC.hasKey` CompatGHC.ipClassKey
    , [name, ty] <- tys
    , Just x <- CompatGHC.isStrLitTy name =
        CompatGHC.noLocA $
          CompatGHC.HsIParamTy
            CompatGHC.noAnn
            (CompatGHC.noLocA $ CompatGHC.HsIPName x)
            (synifyType WithinType vs ty)
    -- and equalities
    | tc `CompatGHC.hasKey` CompatGHC.eqTyConKey
    , [ty1, ty2] <- tys =
        CompatGHC.noLocA $
          CompatGHC.HsOpTy
            CompatGHC.noAnn
            CompatGHC.NotPromoted
            (synifyType WithinType vs ty1)
            (CompatGHC.noLocA CompatGHC.eqTyConName)
            (synifyType WithinType vs ty2)
    -- and infix type operators
    | CompatGHC.isSymOcc (CompatGHC.nameOccName (CompatGHC.getName tc))
    , ty1 : ty2 : tys_rest <- vis_tys =
        mk_app_tys
          ( CompatGHC.HsOpTy
              CompatGHC.noAnn
              prom
              (synifyType WithinType vs ty1)
              (CompatGHC.noLocA $ CompatGHC.getName tc)
              (synifyType WithinType vs ty2)
          )
          tys_rest
    -- Most TyCons:
    | otherwise =
        mk_app_tys
          (CompatGHC.HsTyVar CompatGHC.noAnn prom $ CompatGHC.noLocA (CompatGHC.getName tc))
          vis_tys
   where
    prom = if CompatGHC.isPromotedDataCon tc then CompatGHC.IsPromoted else CompatGHC.NotPromoted
    mk_app_tys ty_app ty_args =
      foldl
        (\t1 t2 -> CompatGHC.noLocA $ CompatGHC.HsAppTy CompatGHC.noExtField t1 t2)
        (CompatGHC.noLocA ty_app)
        ( map (synifyType WithinType vs) $
            CompatGHC.filterOut CompatGHC.isCoercionTy ty_args
        )

  tys_len = length tys
  vis_tys = CompatGHC.filterOutInvisibleTypes tc tys

  maybe_sig :: CompatGHC.LHsType CompatGHC.GhcRn -> CompatGHC.LHsType CompatGHC.GhcRn
  maybe_sig ty'
    | CompatGHC.tyConAppNeedsKindSig False tc tys_len =
        let full_kind = CompatGHC.typeKind (CompatGHC.mkTyConApp tc tys)
            full_kind' = synifyType WithinType vs full_kind
         in CompatGHC.noLocA $ CompatGHC.HsKindSig CompatGHC.noAnn ty' full_kind'
    | otherwise = ty'
synifyType _ vs ty@(CompatGHC.AppTy{}) =
  let
    (ty_head, ty_args) = CompatGHC.splitAppTys ty
    ty_head' = synifyType WithinType vs ty_head
    ty_args' =
      map (synifyType WithinType vs) $
        CompatGHC.filterOut CompatGHC.isCoercionTy $
          CompatGHC.filterByList
            (map CompatGHC.isVisibleArgFlag $ CompatGHC.appTyArgFlags ty_head ty_args)
            ty_args
   in
    foldl (\t1 t2 -> CompatGHC.noLocA $ CompatGHC.HsAppTy CompatGHC.noExtField t1 t2) ty_head' ty_args'
synifyType s vs funty@(CompatGHC.FunTy CompatGHC.InvisArg _ _ _) = synifySigmaType s vs funty
synifyType _ vs (CompatGHC.FunTy CompatGHC.VisArg w t1 t2) =
  let
    s1 = synifyType WithinType vs t1
    s2 = synifyType WithinType vs t2
    w' = synifyMult vs w
   in
    CompatGHC.noLocA $ CompatGHC.HsFunTy CompatGHC.noAnn w' s1 s2
synifyType s vs forallty@(CompatGHC.ForAllTy (CompatGHC.Bndr _ argf) _ty) =
  case argf of
    CompatGHC.Required -> synifyVisForAllType vs forallty
    CompatGHC.Invisible _ -> synifySigmaType s vs forallty
synifyType _ _ (CompatGHC.LitTy t) = CompatGHC.noLocA $ CompatGHC.HsTyLit CompatGHC.noExtField $ synifyTyLit t
synifyType s vs (CompatGHC.CastTy t _) = synifyType s vs t
synifyType _ _ (CompatGHC.CoercionTy{}) = error "synifyType:Coercion"

-- | Process a 'CompatGHC.Type' which starts with a visible @forall@ into an 'CompatGHC.HsType'
synifyVisForAllType ::
  [CompatGHC.TyVar]
  -- ^ free variables in the type to convert
  -> CompatGHC.Type
  -- ^ the forall type to convert
  -> CompatGHC.LHsType CompatGHC.GhcRn
synifyVisForAllType vs ty =
  let (tvs, rho) = tcSplitForAllTysReqPreserveSynonyms ty

      sTvs = map synifyTyVarBndr tvs

      -- Figure out what the type variable order would be inferred in the
      -- absence of an explicit forall
      tvs' = orderedFVs (CompatGHC.mkVarSet vs) [rho]
   in CompatGHC.noLocA $
        CompatGHC.HsForAllTy
          { CompatGHC.hst_tele = CompatGHC.mkHsForAllVisTele CompatGHC.noAnn sTvs
          , CompatGHC.hst_xforall = CompatGHC.noExtField
          , CompatGHC.hst_body = synifyType WithinType (tvs' ++ vs) rho
          }

{- | Process a 'CompatGHC.Type' which starts with an invisible @forall@ or a constraint
into an 'CompatGHC.HsType'
-}
synifySigmaType ::
  SynifyTypeState
  -- ^ what to do with the 'forall'
  -> [CompatGHC.TyVar]
  -- ^ free variables in the type to convert
  -> CompatGHC.Type
  -- ^ the forall type to convert
  -> CompatGHC.LHsType CompatGHC.GhcRn
synifySigmaType s vs ty =
  let (tvs, ctx, tau) = tcSplitSigmaTyPreserveSynonyms ty
      sPhi =
        CompatGHC.HsQualTy
          { CompatGHC.hst_ctxt = synifyCtx ctx
          , CompatGHC.hst_xqual = CompatGHC.noExtField
          , CompatGHC.hst_body = synifyType WithinType (tvs' ++ vs) tau
          }

      sTy =
        CompatGHC.HsForAllTy
          { CompatGHC.hst_tele = CompatGHC.mkHsForAllInvisTele CompatGHC.noAnn sTvs
          , CompatGHC.hst_xforall = CompatGHC.noExtField
          , CompatGHC.hst_body = CompatGHC.noLocA sPhi
          }

      sTvs = map synifyTyVarBndr tvs

      -- Figure out what the type variable order would be inferred in the
      -- absence of an explicit forall
      tvs' = orderedFVs (CompatGHC.mkVarSet vs) (ctx ++ [tau])
   in case s of
        DeleteTopLevelQuantification -> synifyType ImplicitizeForAll (tvs' ++ vs) tau
        -- Put a forall in if there are any type variables
        WithinType
          | not (null tvs) -> CompatGHC.noLocA sTy
          | otherwise -> CompatGHC.noLocA sPhi
        ImplicitizeForAll -> implicitForAll [] vs tvs ctx (synifyType WithinType) tau

{- | Put a forall in if there are any type variables which require
explicit kind annotations or if the inferred type variable order
would be different.
-}
implicitForAll ::
  [CompatGHC.TyCon]
  -- ^ type constructors that determine their args kinds
  -> [CompatGHC.TyVar]
  -- ^ free variables in the type to convert
  -> [CompatGHC.InvisTVBinder]
  -- ^ type variable binders in the forall
  -> CompatGHC.ThetaType
  -- ^ constraints right after the forall
  -> ([CompatGHC.TyVar] -> CompatGHC.Type -> CompatGHC.LHsType CompatGHC.GhcRn)
  -- ^ how to convert the inner type
  -> CompatGHC.Type
  -- ^ inner type
  -> CompatGHC.LHsType CompatGHC.GhcRn
implicitForAll tycons vs tvs ctx synInner tau
  | any (CompatGHC.isHsKindedTyVar . CompatGHC.unLoc) sTvs = CompatGHC.noLocA sTy
  | tvs' /= (CompatGHC.binderVars tvs) = CompatGHC.noLocA sTy
  | otherwise = CompatGHC.noLocA sPhi
 where
  sRho = synInner (tvs' ++ vs) tau
  sPhi
    | null ctx = CompatGHC.unLoc sRho
    | otherwise =
        CompatGHC.HsQualTy
          { CompatGHC.hst_ctxt = synifyCtx ctx
          , CompatGHC.hst_xqual = CompatGHC.noExtField
          , CompatGHC.hst_body = synInner (tvs' ++ vs) tau
          }
  sTy =
    CompatGHC.HsForAllTy
      { CompatGHC.hst_tele = CompatGHC.mkHsForAllInvisTele CompatGHC.noAnn sTvs
      , CompatGHC.hst_xforall = CompatGHC.noExtField
      , CompatGHC.hst_body = CompatGHC.noLocA sPhi
      }

  no_kinds_needed = noKindTyVars tycons tau
  sTvs = map (synifyTyVarBndr' no_kinds_needed) tvs

  -- Figure out what the type variable order would be inferred in the
  -- absence of an explicit forall
  tvs' = orderedFVs (CompatGHC.mkVarSet vs) (ctx ++ [tau])

{- | Find the set of type variables whose kind signatures can be properly
inferred just from their uses in the type signature. This means the type
variable to has at least one fully applied use @f x1 x2 ... xn@ where:

  * @f@ has a function kind where the arguments have the same kinds
    as @x1 x2 ... xn@.

  * @f@ has a function kind whose final return has lifted type kind
-}
noKindTyVars ::
  [CompatGHC.TyCon]
  -- ^ type constructors that determine their args kinds
  -> CompatGHC.Type
  -- ^ type to inspect
  -> CompatGHC.VarSet
  -- ^ set of variables whose kinds can be inferred from uses in the type
noKindTyVars _ (CompatGHC.TyVarTy var)
  | CompatGHC.isLiftedTypeKind (CompatGHC.tyVarKind var) = CompatGHC.unitVarSet var
noKindTyVars ts ty
  | (f, xs) <- CompatGHC.splitAppTys ty
  , not (null xs) =
      let args = map (noKindTyVars ts) xs
          func = case f of
            CompatGHC.TyVarTy var
              | (xsKinds, outKind) <- CompatGHC.splitFunTys (CompatGHC.tyVarKind var)
              , map CompatGHC.scaledThing xsKinds `CompatGHC.eqTypes` map CompatGHC.typeKind xs
              , CompatGHC.isLiftedTypeKind outKind ->
                  CompatGHC.unitVarSet var
            CompatGHC.TyConApp t ks
              | t `elem` ts
              , all CompatGHC.noFreeVarsOfType ks ->
                  CompatGHC.mkVarSet [v | CompatGHC.TyVarTy v <- xs]
            _ -> noKindTyVars ts f
       in CompatGHC.unionVarSets (func : args)
noKindTyVars ts (CompatGHC.ForAllTy _ t) = noKindTyVars ts t
noKindTyVars ts (CompatGHC.FunTy _ w t1 t2) =
  noKindTyVars ts w
    `CompatGHC.unionVarSet` noKindTyVars ts t1
    `CompatGHC.unionVarSet` noKindTyVars ts t2
noKindTyVars ts (CompatGHC.CastTy t _) = noKindTyVars ts t
noKindTyVars _ _ = CompatGHC.emptyVarSet

synifyMult :: [CompatGHC.TyVar] -> CompatGHC.Mult -> CompatGHC.HsArrow CompatGHC.GhcRn
synifyMult vs t = case t of
  CompatGHC.One -> CompatGHC.HsLinearArrow (CompatGHC.HsPct1 CompatGHC.noHsTok CompatGHC.noHsUniTok)
  CompatGHC.Many -> CompatGHC.HsUnrestrictedArrow CompatGHC.noHsUniTok
  ty -> CompatGHC.HsExplicitMult CompatGHC.noHsTok (synifyType WithinType vs ty) CompatGHC.noHsUniTok

synifyPatSynType :: CompatGHC.PatSyn -> CompatGHC.LHsType CompatGHC.GhcRn
synifyPatSynType ps =
  let (univ_tvs, req_theta, ex_tvs, prov_theta, arg_tys, res_ty) = CompatGHC.patSynSigBndr ps
      ts = maybeToList (CompatGHC.tyConAppTyCon_maybe res_ty)

      -- HACK: a HsQualTy with theta = [unitTy] will be printed as "() =>",
      -- i.e., an explicit empty context, which is what we need. This is not
      -- possible by taking theta = [], as that will print no context at all
      req_theta'
        | null req_theta
        , not (null prov_theta && null ex_tvs) =
            [CompatGHC.unitTy]
        | otherwise = req_theta
   in implicitForAll
        ts
        []
        (univ_tvs ++ ex_tvs)
        req_theta'
        (\vs -> implicitForAll ts vs [] prov_theta (synifyType WithinType))
        (CompatGHC.mkVisFunTys arg_tys res_ty)

synifyTyLit :: CompatGHC.TyLit -> CompatGHC.HsTyLit
synifyTyLit (CompatGHC.NumTyLit n) = CompatGHC.HsNumTy CompatGHC.NoSourceText n
synifyTyLit (CompatGHC.StrTyLit s) = CompatGHC.HsStrTy CompatGHC.NoSourceText s
synifyTyLit (CompatGHC.CharTyLit c) = CompatGHC.HsCharTy CompatGHC.NoSourceText c

synifyKindSig :: CompatGHC.Kind -> CompatGHC.LHsKind CompatGHC.GhcRn
synifyKindSig k = synifyType WithinType [] k

stripKindSig :: CompatGHC.LHsType CompatGHC.GhcRn -> CompatGHC.LHsType CompatGHC.GhcRn
stripKindSig (CompatGHC.L _ (CompatGHC.HsKindSig _ t _)) = t
stripKindSig t = t

{-
Note [Invariant: Never expand type synonyms]

In haddock, we never want to expand a type synonym that may be presented to the
user, as we want to keep the link to the abstraction captured in the synonym.

All code in Haddock.Convert must make sure that this invariant holds.

See https://github.com/haskell/haddock/issues/879 for a bug where this
invariant didn't hold.
-}

{- | A version of 'TcType.tcSplitSigmaTy' that:

1. Preserves type synonyms.
2. Returns 'CompatGHC.InvisTVBinder's instead of 'CompatGHC.TyVar's.

See Note [Invariant: Never expand type synonyms]
-}
tcSplitSigmaTyPreserveSynonyms ::
  CompatGHC.Type -> ([CompatGHC.InvisTVBinder], CompatGHC.ThetaType, CompatGHC.Type)
tcSplitSigmaTyPreserveSynonyms ty =
  case tcSplitForAllTysInvisPreserveSynonyms ty of
    (tvs, rho) -> case tcSplitPhiTyPreserveSynonyms rho of
      (theta, tau) -> (tvs, theta, tau)

-- | See Note [Invariant: Never expand type synonyms]
tcSplitSomeForAllTysPreserveSynonyms ::
  (CompatGHC.ArgFlag -> Bool) -> CompatGHC.Type -> ([CompatGHC.TyCoVarBinder], CompatGHC.Type)
tcSplitSomeForAllTysPreserveSynonyms argf_pred ty = split ty ty []
 where
  split _ (CompatGHC.ForAllTy tvb@(CompatGHC.Bndr _ argf) ty') tvs
    | argf_pred argf = split ty' ty' (tvb : tvs)
  split orig_ty _ tvs = (reverse tvs, orig_ty)

-- | See Note [Invariant: Never expand type synonyms]
tcSplitForAllTysReqPreserveSynonyms :: CompatGHC.Type -> ([CompatGHC.ReqTVBinder], CompatGHC.Type)
tcSplitForAllTysReqPreserveSynonyms ty =
  let (all_bndrs, body) = tcSplitSomeForAllTysPreserveSynonyms CompatGHC.isVisibleArgFlag ty
      req_bndrs = mapMaybe mk_req_bndr_maybe all_bndrs
   in CompatGHC.assert
        (req_bndrs `CompatGHC.equalLength` all_bndrs)
        (req_bndrs, body)
 where
  mk_req_bndr_maybe :: CompatGHC.TyCoVarBinder -> Maybe CompatGHC.ReqTVBinder
  mk_req_bndr_maybe (CompatGHC.Bndr tv argf) = case argf of
    CompatGHC.Required -> Just $ CompatGHC.Bndr tv ()
    CompatGHC.Invisible _ -> Nothing

-- | See Note [Invariant: Never expand type synonyms]
tcSplitForAllTysInvisPreserveSynonyms ::
  CompatGHC.Type -> ([CompatGHC.InvisTVBinder], CompatGHC.Type)
tcSplitForAllTysInvisPreserveSynonyms ty =
  let (all_bndrs, body) = tcSplitSomeForAllTysPreserveSynonyms CompatGHC.isInvisibleArgFlag ty
      inv_bndrs = mapMaybe mk_inv_bndr_maybe all_bndrs
   in CompatGHC.assert
        (inv_bndrs `CompatGHC.equalLength` all_bndrs)
        (inv_bndrs, body)
 where
  mk_inv_bndr_maybe :: CompatGHC.TyCoVarBinder -> Maybe CompatGHC.InvisTVBinder
  mk_inv_bndr_maybe (CompatGHC.Bndr tv argf) = case argf of
    CompatGHC.Invisible s -> Just $ CompatGHC.Bndr tv s
    CompatGHC.Required -> Nothing

-- | See Note [Invariant: Never expand type synonyms]

-- | See Note [Invariant: Never expand type synonyms]
tcSplitPhiTyPreserveSynonyms :: CompatGHC.Type -> (CompatGHC.ThetaType, CompatGHC.Type)
tcSplitPhiTyPreserveSynonyms ty0 = split ty0 []
 where
  split ty ts =
    case tcSplitPredFunTyPreserveSynonyms_maybe ty of
      Just (pred_, ty') -> split ty' (pred_ : ts)
      Nothing -> (reverse ts, ty)

-- | See Note [Invariant: Never expand type synonyms]
tcSplitPredFunTyPreserveSynonyms_maybe ::
  CompatGHC.Type -> Maybe (CompatGHC.PredType, CompatGHC.Type)
tcSplitPredFunTyPreserveSynonyms_maybe (CompatGHC.FunTy CompatGHC.InvisArg _ arg res) = Just (arg, res)
tcSplitPredFunTyPreserveSynonyms_maybe _ = Nothing
