{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      :  Haddock.GhcUtils
Copyright   :  (c) David Waern 2006-2009
License     :  BSD-like

Maintainer  :  haddock@projects.haskell.org
Stability   :  experimental
Portability :  portable

Utils for dealing with types from the GHC API
-}
module Haddock.GhcUtils where

import qualified Data.Maybe as Maybe

import qualified Pollock.CompatGHC as CompatGHC

-- Useful when there is a signature with multiple names, e.g.
--   foo, bar :: Types..
-- but only one of the names is exported and we have to change the
-- type signature to only include the exported names.
filterLSigNames ::
  (CompatGHC.IdP (CompatGHC.GhcPass p) -> Bool) -> CompatGHC.LSig (CompatGHC.GhcPass p) -> Maybe (CompatGHC.LSig (CompatGHC.GhcPass p))
filterLSigNames p (CompatGHC.L loc sig) = CompatGHC.L loc <$> (filterSigNames p sig)

filterSigNames :: (CompatGHC.IdP (CompatGHC.GhcPass p) -> Bool) -> CompatGHC.Sig (CompatGHC.GhcPass p) -> Maybe (CompatGHC.Sig (CompatGHC.GhcPass p))
filterSigNames p orig@(CompatGHC.SpecSig _ n _ _) = ifTrueJust (p $ CompatGHC.unLoc n) orig
filterSigNames p orig@(CompatGHC.InlineSig _ n _) = ifTrueJust (p $ CompatGHC.unLoc n) orig
filterSigNames p (CompatGHC.FixSig _ (CompatGHC.FixitySig _ ns ty)) =
  case filter (p . CompatGHC.unLoc) ns of
    [] -> Nothing
    filtered -> Just (CompatGHC.FixSig CompatGHC.noAnn (CompatGHC.FixitySig CompatGHC.noExtField filtered ty))
filterSigNames _ orig@(CompatGHC.MinimalSig _ _ _) = Just orig
filterSigNames p (CompatGHC.TypeSig _ ns ty) =
  case filter (p . CompatGHC.unLoc) ns of
    [] -> Nothing
    filtered -> Just (CompatGHC.TypeSig CompatGHC.noAnn filtered ty)
filterSigNames p (CompatGHC.ClassOpSig _ is_default ns ty) =
  case filter (p . CompatGHC.unLoc) ns of
    [] -> Nothing
    filtered -> Just (CompatGHC.ClassOpSig CompatGHC.noAnn is_default filtered ty)
filterSigNames p (CompatGHC.PatSynSig _ ns ty) =
  case filter (p . CompatGHC.unLoc) ns of
    [] -> Nothing
    filtered -> Just (CompatGHC.PatSynSig CompatGHC.noAnn filtered ty)
filterSigNames _ _ = Nothing

ifTrueJust :: Bool -> name -> Maybe name
ifTrueJust True = Just
ifTrueJust False = const Nothing

sigName :: CompatGHC.LSig CompatGHC.GhcRn -> [CompatGHC.IdP CompatGHC.GhcRn]
sigName (CompatGHC.L _ sig) = CompatGHC.sigNameNoLoc CompatGHC.emptyOccEnv sig

pretty :: (CompatGHC.Outputable a) => CompatGHC.DynFlags -> a -> String
pretty = CompatGHC.showPpr

-- ---------------------------------------------------------------------

-- These functions are duplicated from the GHC API, as they must be
-- instantiated at DocNameI instead of (GhcPass _).

mkEmptySigType :: CompatGHC.LHsType CompatGHC.GhcRn -> CompatGHC.LHsSigType CompatGHC.GhcRn
-- Dubious, because the implicit binders are empty even
-- though the type might have free variables
mkEmptySigType lty@(CompatGHC.L loc ty) = CompatGHC.L loc $ case ty of
  CompatGHC.HsForAllTy
    { CompatGHC.hst_tele = CompatGHC.HsForAllInvis{CompatGHC.hsf_invis_bndrs = bndrs}
    , CompatGHC.hst_body = body
    } ->
      CompatGHC.HsSig
        { CompatGHC.sig_ext = CompatGHC.noExtField
        , CompatGHC.sig_bndrs =
            CompatGHC.HsOuterExplicit
              { CompatGHC.hso_xexplicit = CompatGHC.noExtField
              , CompatGHC.hso_bndrs = bndrs
              }
        , CompatGHC.sig_body = body
        }
  _ ->
    CompatGHC.HsSig
      { CompatGHC.sig_ext = CompatGHC.noExtField
      , CompatGHC.sig_bndrs = CompatGHC.HsOuterImplicit{CompatGHC.hso_ximplicit = []}
      , CompatGHC.sig_body = lty
      }

addClassContext ::
  CompatGHC.Name -> CompatGHC.LHsQTyVars CompatGHC.GhcRn -> CompatGHC.LSig CompatGHC.GhcRn -> CompatGHC.LSig CompatGHC.GhcRn
-- Add the class context to a class-op signature
addClassContext cls tvs0 (CompatGHC.L pos (CompatGHC.ClassOpSig _ _ lname ltype)) =
  CompatGHC.L pos (CompatGHC.TypeSig CompatGHC.noAnn lname (CompatGHC.mkEmptyWildCardBndrs (go_sig_ty ltype)))
 where
  go_sig_ty (CompatGHC.L loc (CompatGHC.HsSig{CompatGHC.sig_bndrs = bndrs, CompatGHC.sig_body = ty})) =
    CompatGHC.L
      loc
      ( CompatGHC.HsSig
          { CompatGHC.sig_ext = CompatGHC.noExtField
          , CompatGHC.sig_bndrs = bndrs
          , CompatGHC.sig_body = go_ty ty
          }
      )

  go_ty (CompatGHC.L loc (CompatGHC.HsForAllTy{CompatGHC.hst_tele = tele, CompatGHC.hst_body = ty})) =
    CompatGHC.L
      loc
      ( CompatGHC.HsForAllTy
          { CompatGHC.hst_xforall = CompatGHC.noExtField
          , CompatGHC.hst_tele = tele
          , CompatGHC.hst_body = go_ty ty
          }
      )
  go_ty (CompatGHC.L loc (CompatGHC.HsQualTy{CompatGHC.hst_ctxt = ctxt, CompatGHC.hst_body = ty})) =
    CompatGHC.L
      loc
      ( CompatGHC.HsQualTy
          { CompatGHC.hst_xqual = CompatGHC.noExtField
          , CompatGHC.hst_ctxt = add_ctxt ctxt
          , CompatGHC.hst_body = ty
          }
      )
  go_ty (CompatGHC.L loc ty) =
    CompatGHC.L
      loc
      ( CompatGHC.HsQualTy
          { CompatGHC.hst_xqual = CompatGHC.noExtField
          , CompatGHC.hst_ctxt = add_ctxt (CompatGHC.noLocA [])
          , CompatGHC.hst_body = CompatGHC.L loc ty
          }
      )

  extra_pred = CompatGHC.nlHsTyConApp CompatGHC.NotPromoted CompatGHC.Prefix cls (lHsQTyVarsToTypes tvs0)

  add_ctxt (CompatGHC.L loc preds) = CompatGHC.L loc (extra_pred : preds)
addClassContext _ _ sig = sig -- E.g. a MinimalSig is fine

lHsQTyVarsToTypes :: CompatGHC.LHsQTyVars CompatGHC.GhcRn -> [CompatGHC.LHsTypeArg CompatGHC.GhcRn]
lHsQTyVarsToTypes tvs =
  [ CompatGHC.HsValArg $ CompatGHC.noLocA (CompatGHC.HsTyVar CompatGHC.noAnn CompatGHC.NotPromoted (CompatGHC.noLocA (CompatGHC.hsLTyVarName tv)))
  | tv <- CompatGHC.hsQTvExplicit tvs
  ]

--------------------------------------------------------------------------------

-- * Making abstract declarations

--------------------------------------------------------------------------------

restrictTo ::
  [CompatGHC.Name] -> CompatGHC.LHsDecl CompatGHC.GhcRn -> CompatGHC.LHsDecl CompatGHC.GhcRn
restrictTo names (CompatGHC.L loc decl) = CompatGHC.L loc $ case decl of
  CompatGHC.TyClD x d
    | CompatGHC.isDataDecl d ->
        CompatGHC.TyClD x (d{CompatGHC.tcdDataDefn = restrictDataDefn names (CompatGHC.tcdDataDefn d)})
  CompatGHC.TyClD x d
    | CompatGHC.isClassDecl d ->
        CompatGHC.TyClD
          x
          ( d
              { CompatGHC.tcdSigs = restrictDecls names (CompatGHC.tcdSigs d)
              , CompatGHC.tcdATs = restrictATs names (CompatGHC.tcdATs d)
              }
          )
  _ -> decl

restrictDataDefn :: [CompatGHC.Name] -> CompatGHC.HsDataDefn CompatGHC.GhcRn -> CompatGHC.HsDataDefn CompatGHC.GhcRn
restrictDataDefn names defn@(CompatGHC.HsDataDefn{CompatGHC.dd_ND = new_or_data, CompatGHC.dd_cons = cons})
  | CompatGHC.DataType <- new_or_data =
      defn{CompatGHC.dd_cons = restrictCons names cons}
  | otherwise -- Newtype
    =
      case restrictCons names cons of
        [] -> defn{CompatGHC.dd_ND = CompatGHC.DataType, CompatGHC.dd_cons = []}
        [con] -> defn{CompatGHC.dd_cons = [con]}
        _ -> error "Should not happen"

restrictCons :: [CompatGHC.Name] -> [CompatGHC.LConDecl CompatGHC.GhcRn] -> [CompatGHC.LConDecl CompatGHC.GhcRn]
restrictCons names decls = [CompatGHC.L p d | CompatGHC.L p (Just d) <- map (fmap keep) decls]
 where
  keep :: CompatGHC.ConDecl CompatGHC.GhcRn -> Maybe (CompatGHC.ConDecl CompatGHC.GhcRn)
  keep d
    | any ((\n -> n `elem` names) . CompatGHC.unLoc) (CompatGHC.getConNames d) =
        case d of
          CompatGHC.ConDeclH98{CompatGHC.con_args = con_args'} -> case con_args' of
            CompatGHC.PrefixCon{} -> Just d
            CompatGHC.RecCon fields
              | all field_avail (CompatGHC.unLoc fields) -> Just d
              | otherwise -> Just (d{CompatGHC.con_args = CompatGHC.PrefixCon [] (field_types $ CompatGHC.unLoc fields)})
            -- if we have *all* the field names available, then
            -- keep the record declaration.  Otherwise degrade to
            -- a constructor declaration.  This isn't quite right, but
            -- it's the best we can do.
            CompatGHC.InfixCon _ _ -> Just d
          CompatGHC.ConDeclGADT{CompatGHC.con_g_args = con_args'} -> case con_args' of
            CompatGHC.PrefixConGADT{} -> Just d
            CompatGHC.RecConGADT fields _
              | all field_avail (CompatGHC.unLoc fields) -> Just d
              | otherwise -> Just (d{CompatGHC.con_g_args = CompatGHC.PrefixConGADT (field_types $ CompatGHC.unLoc fields)})
   where
    -- see above

    field_avail :: CompatGHC.LConDeclField CompatGHC.GhcRn -> Bool
    field_avail (CompatGHC.L _ (CompatGHC.ConDeclField _ fs _ _)) =
      all (\f -> CompatGHC.foExt (CompatGHC.unLoc f) `elem` names) fs

    field_types flds = [CompatGHC.hsUnrestricted t | CompatGHC.L _ (CompatGHC.ConDeclField _ _ t _) <- flds]
  keep _ = Nothing

restrictDecls :: [CompatGHC.Name] -> [CompatGHC.LSig CompatGHC.GhcRn] -> [CompatGHC.LSig CompatGHC.GhcRn]
restrictDecls names = Maybe.mapMaybe (filterLSigNames (`elem` names))

restrictATs :: [CompatGHC.Name] -> [CompatGHC.LFamilyDecl CompatGHC.GhcRn] -> [CompatGHC.LFamilyDecl CompatGHC.GhcRn]
restrictATs names ats = [at | at <- ats, CompatGHC.unLoc (CompatGHC.fdLName (CompatGHC.unLoc at)) `elem` names]

-------------------------------------------------------------------------------

-- * Free variables of a 'CompatGHC.Type'

-------------------------------------------------------------------------------

{- | Get free type variables inCompatGHC.TypeType' in their order of appearance.
See [Ordering of implicit variables].
-}
orderedFVs ::
  CompatGHC.VarSet
  -- ^ free variables to ignore
  -> [CompatGHC.Type]
  -- ^ types to traverse (in order) looking for free variables
  -> [CompatGHC.TyVar]
  -- ^ free type variables, in the order they appear in
orderedFVs vs tys =
  reverse . fst $ tyCoFVsOfTypes' tys (const True) vs ([], CompatGHC.emptyVarSet)

-- See the "Free variables of types and coercions" section in 'TyCoRep', or
-- check out Note [Free variables of types]. The functions in this section
-- don't output type variables in the order they first appear in in the 'Type'.
--
-- For example, 'tyCoVarsOfTypeList' reports an incorrect order for the type
-- of 'const :: a -> b -> a':
--
-- >>> import GHC.Types.Name
-- >>> import TyCoRep
-- >>> import GHC.Builtin.Types.Prim
-- >>> import GHC.Types.Var
-- >>> a = TyVarTy alphaTyVar
-- >>> b = TyVarTy betaTyVar
-- >>> constTy = mkFunTys [a, b] a
-- >>> map (getOccString . tyVarName) (tyCoVarsOfTypeList constTy)
-- ["b","a"]
--
-- However, we want to reuse the very optimized traversal machinery there, so
-- so we make our own `tyCoFVsOfType'`, `tyCoFVsBndr'`, and `tyCoVarsOfTypes'`.
-- All these do differently is traverse in a different order and ignore
-- coercion variables.

{- | Just like 'tyCoFVsOfType', but traverses type variables in reverse order
of  appearance.
-}
tyCoFVsOfType' :: CompatGHC.Type -> CompatGHC.FV
tyCoFVsOfType' (CompatGHC.TyVarTy v) a b c = (CompatGHC.unitFV v `CompatGHC.unionFV` tyCoFVsOfType' (CompatGHC.tyVarKind v)) a b c
tyCoFVsOfType' (CompatGHC.TyConApp _ tys) a b c = tyCoFVsOfTypes' tys a b c
tyCoFVsOfType' (CompatGHC.LitTy{}) a b c = CompatGHC.emptyFV a b c
tyCoFVsOfType' (CompatGHC.AppTy fun arg) a b c = (tyCoFVsOfType' arg `CompatGHC.unionFV` tyCoFVsOfType' fun) a b c
tyCoFVsOfType' (CompatGHC.FunTy _ w arg res) a b c =
  ( tyCoFVsOfType' w
      `CompatGHC.unionFV` tyCoFVsOfType' res
      `CompatGHC.unionFV` tyCoFVsOfType' arg
  )
    a
    b
    c
tyCoFVsOfType' (CompatGHC.ForAllTy bndr ty) a b c = tyCoFVsBndr' bndr (tyCoFVsOfType' ty) a b c
tyCoFVsOfType' (CompatGHC.CastTy ty _) a b c = (tyCoFVsOfType' ty) a b c
tyCoFVsOfType' (CompatGHC.CoercionTy _) a b c = CompatGHC.emptyFV a b c

{- | Just like 'tyCoFVsOfTypes', but traverses type variables in reverse order
of appearance.
-}
tyCoFVsOfTypes' :: [CompatGHC.Type] -> CompatGHC.FV
tyCoFVsOfTypes' (ty : tys) fv_cand in_scope acc = (tyCoFVsOfTypes' tys `CompatGHC.unionFV` tyCoFVsOfType' ty) fv_cand in_scope acc
tyCoFVsOfTypes' [] fv_cand in_scope acc = CompatGHC.emptyFV fv_cand in_scope acc

{- | Just like 'tyCoFVsBndr', but traverses type variables in reverse order of
appearance.
-}
tyCoFVsBndr' :: CompatGHC.TyVarBinder -> CompatGHC.FV -> CompatGHC.FV
tyCoFVsBndr' (CompatGHC.Bndr tv _) fvs = CompatGHC.delFV tv fvs `CompatGHC.unionFV` tyCoFVsOfType' (CompatGHC.tyVarKind tv)

-------------------------------------------------------------------------------

-- * Defaulting RuntimeRep variables

-------------------------------------------------------------------------------

{- | Traverses the type, defaulting type variables of kind 'RuntimeRep' to
'LiftedType'. See 'defaultRuntimeRepVars' in GHC.Iface.Type the original such
function working over `IfaceType`'s.
-}
defaultRuntimeRepVars :: CompatGHC.Type -> CompatGHC.Type
defaultRuntimeRepVars = go CompatGHC.emptyVarEnv
 where
  go :: CompatGHC.TyVarEnv () -> CompatGHC.Type -> CompatGHC.Type
  go subs (CompatGHC.ForAllTy (CompatGHC.Bndr var flg) ty)
    | CompatGHC.isRuntimeRepVar var
    , CompatGHC.isInvisibleArgFlag flg =
        let subs' = CompatGHC.extendVarEnv subs var ()
         in go subs' ty
    | otherwise =
        CompatGHC.ForAllTy
          (CompatGHC.Bndr (CompatGHC.updateTyVarKind (go subs) var) flg)
          (go subs ty)
  go subs (CompatGHC.TyVarTy tv)
    | tv `CompatGHC.elemVarEnv` subs =
        CompatGHC.liftedRepTy
    | otherwise =
        CompatGHC.TyVarTy (CompatGHC.updateTyVarKind (go subs) tv)
  go subs (CompatGHC.TyConApp tc tc_args) =
    CompatGHC.TyConApp tc (map (go subs) tc_args)
  go subs (CompatGHC.FunTy af w arg res) =
    CompatGHC.FunTy af (go subs w) (go subs arg) (go subs res)
  go subs (CompatGHC.AppTy t u) =
    CompatGHC.AppTy (go subs t) (go subs u)
  go subs (CompatGHC.CastTy x co) =
    CompatGHC.CastTy (go subs x) co
  go _ ty@(CompatGHC.LitTy{}) = ty
  go _ ty@(CompatGHC.CoercionTy{}) = ty
