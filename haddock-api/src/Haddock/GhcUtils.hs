{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

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

import Control.Arrow
import Data.Maybe (mapMaybe)

import GHC
import GHC.Builtin.Types (liftedRepTy)
import GHC.Core.TyCo.Rep (Type (..))
import GHC.Core.Type (isRuntimeRepVar)
import GHC.Driver.Ppr (showPpr)
import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Var
  ( TyVarBinder
  , VarBndr (..)
  , isInvisibleArgFlag
  , tyVarKind
  , updateTyVarKind
  )
import GHC.Types.Var.Env (TyVarEnv, elemVarEnv, emptyVarEnv, extendVarEnv)
import GHC.Types.Var.Set (VarSet, emptyVarSet)
import GHC.Unit.Module
import GHC.Utils.FV as FV
import GHC.Utils.Outputable (Outputable)

import GHC.HsToCore.Docs

moduleString :: Module -> String
moduleString = moduleNameString . moduleName

-- Useful when there is a signature with multiple names, e.g.
--   foo, bar :: Types..
-- but only one of the names is exported and we have to change the
-- type signature to only include the exported names.
filterLSigNames :: (IdP (GhcPass p) -> Bool) -> LSig (GhcPass p) -> Maybe (LSig (GhcPass p))
filterLSigNames p (L loc sig) = L loc <$> (filterSigNames p sig)

filterSigNames :: (IdP (GhcPass p) -> Bool) -> Sig (GhcPass p) -> Maybe (Sig (GhcPass p))
filterSigNames p orig@(SpecSig _ n _ _) = ifTrueJust (p $ unLoc n) orig
filterSigNames p orig@(InlineSig _ n _) = ifTrueJust (p $ unLoc n) orig
filterSigNames p (FixSig _ (FixitySig _ ns ty)) =
  case filter (p . unLoc) ns of
    [] -> Nothing
    filtered -> Just (FixSig noAnn (FixitySig noExtField filtered ty))
filterSigNames _ orig@(MinimalSig _ _ _) = Just orig
filterSigNames p (TypeSig _ ns ty) =
  case filter (p . unLoc) ns of
    [] -> Nothing
    filtered -> Just (TypeSig noAnn filtered ty)
filterSigNames p (ClassOpSig _ is_default ns ty) =
  case filter (p . unLoc) ns of
    [] -> Nothing
    filtered -> Just (ClassOpSig noAnn is_default filtered ty)
filterSigNames p (PatSynSig _ ns ty) =
  case filter (p . unLoc) ns of
    [] -> Nothing
    filtered -> Just (PatSynSig noAnn filtered ty)
filterSigNames _ _ = Nothing

ifTrueJust :: Bool -> name -> Maybe name
ifTrueJust True = Just
ifTrueJust False = const Nothing

sigName :: LSig GhcRn -> [IdP GhcRn]
sigName (L _ sig) = sigNameNoLoc emptyOccEnv sig

pretty :: (Outputable a) => DynFlags -> a -> String
pretty = showPpr

-- ---------------------------------------------------------------------

-- These functions are duplicated from the GHC API, as they must be
-- instantiated at DocNameI instead of (GhcPass _).

mkEmptySigType :: LHsType GhcRn -> LHsSigType GhcRn
-- Dubious, because the implicit binders are empty even
-- though the type might have free variables
mkEmptySigType lty@(L loc ty) = L loc $ case ty of
  HsForAllTy
    { hst_tele = HsForAllInvis{hsf_invis_bndrs = bndrs}
    , hst_body = body
    } ->
      HsSig
        { sig_ext = noExtField
        , sig_bndrs =
            HsOuterExplicit
              { hso_xexplicit = noExtField
              , hso_bndrs = bndrs
              }
        , sig_body = body
        }
  _ ->
    HsSig
      { sig_ext = noExtField
      , sig_bndrs = HsOuterImplicit{hso_ximplicit = []}
      , sig_body = lty
      }

addClassContext :: Name -> LHsQTyVars GhcRn -> LSig GhcRn -> LSig GhcRn
-- Add the class context to a class-op signature
addClassContext cls tvs0 (L pos (ClassOpSig _ _ lname ltype)) =
  L pos (TypeSig noAnn lname (mkEmptyWildCardBndrs (go_sig_ty ltype)))
 where
  go_sig_ty (L loc (HsSig{sig_bndrs = bndrs, sig_body = ty})) =
    L
      loc
      ( HsSig
          { sig_ext = noExtField
          , sig_bndrs = bndrs
          , sig_body = go_ty ty
          }
      )

  go_ty (L loc (HsForAllTy{hst_tele = tele, hst_body = ty})) =
    L
      loc
      ( HsForAllTy
          { hst_xforall = noExtField
          , hst_tele = tele
          , hst_body = go_ty ty
          }
      )
  go_ty (L loc (HsQualTy{hst_ctxt = ctxt, hst_body = ty})) =
    L
      loc
      ( HsQualTy
          { hst_xqual = noExtField
          , hst_ctxt = add_ctxt ctxt
          , hst_body = ty
          }
      )
  go_ty (L loc ty) =
    L
      loc
      ( HsQualTy
          { hst_xqual = noExtField
          , hst_ctxt = add_ctxt (noLocA [])
          , hst_body = L loc ty
          }
      )

  extra_pred = nlHsTyConApp NotPromoted Prefix cls (lHsQTyVarsToTypes tvs0)

  add_ctxt (L loc preds) = L loc (extra_pred : preds)
addClassContext _ _ sig = sig -- E.g. a MinimalSig is fine

lHsQTyVarsToTypes :: LHsQTyVars GhcRn -> [LHsTypeArg GhcRn]
lHsQTyVarsToTypes tvs =
  [ HsValArg $ noLocA (HsTyVar noAnn NotPromoted (noLocA (hsLTyVarName tv)))
  | tv <- hsQTvExplicit tvs
  ]

--------------------------------------------------------------------------------

-- * Making abstract declarations

--------------------------------------------------------------------------------

restrictTo :: [Name] -> LHsDecl GhcRn -> LHsDecl GhcRn
restrictTo names (L loc decl) = L loc $ case decl of
  TyClD x d
    | isDataDecl d ->
        TyClD x (d{tcdDataDefn = restrictDataDefn names (tcdDataDefn d)})
  TyClD x d
    | isClassDecl d ->
        TyClD
          x
          ( d
              { tcdSigs = restrictDecls names (tcdSigs d)
              , tcdATs = restrictATs names (tcdATs d)
              }
          )
  _ -> decl

restrictDataDefn :: [Name] -> HsDataDefn GhcRn -> HsDataDefn GhcRn
restrictDataDefn names defn@(HsDataDefn{dd_ND = new_or_data, dd_cons = cons})
  | DataType <- new_or_data =
      defn{dd_cons = restrictCons names cons}
  | otherwise -- Newtype
    =
      case restrictCons names cons of
        [] -> defn{dd_ND = DataType, dd_cons = []}
        [con] -> defn{dd_cons = [con]}
        _ -> error "Should not happen"

restrictCons :: [Name] -> [LConDecl GhcRn] -> [LConDecl GhcRn]
restrictCons names decls = [L p d | L p (Just d) <- map (fmap keep) decls]
 where
  keep :: ConDecl GhcRn -> Maybe (ConDecl GhcRn)
  keep d
    | any (\n -> n `elem` names) (map unLoc $ getConNames d) =
        case d of
          ConDeclH98{con_args = con_args'} -> case con_args' of
            PrefixCon{} -> Just d
            RecCon fields
              | all field_avail (unLoc fields) -> Just d
              | otherwise -> Just (d{con_args = PrefixCon [] (field_types $ unLoc fields)})
            -- if we have *all* the field names available, then
            -- keep the record declaration.  Otherwise degrade to
            -- a constructor declaration.  This isn't quite right, but
            -- it's the best we can do.
            InfixCon _ _ -> Just d
          ConDeclGADT{con_g_args = con_args'} -> case con_args' of
            PrefixConGADT{} -> Just d
            RecConGADT fields _
              | all field_avail (unLoc fields) -> Just d
              | otherwise -> Just (d{con_g_args = PrefixConGADT (field_types $ unLoc fields)})
   where
    -- see above

    field_avail :: LConDeclField GhcRn -> Bool
    field_avail (L _ (ConDeclField _ fs _ _)) =
      all (\f -> foExt (unLoc f) `elem` names) fs

    field_types flds = [hsUnrestricted t | L _ (ConDeclField _ _ t _) <- flds]
  keep _ = Nothing

restrictDecls :: [Name] -> [LSig GhcRn] -> [LSig GhcRn]
restrictDecls names = mapMaybe (filterLSigNames (`elem` names))

restrictATs :: [Name] -> [LFamilyDecl GhcRn] -> [LFamilyDecl GhcRn]
restrictATs names ats = [at | at <- ats, unLoc (fdLName (unLoc at)) `elem` names]

-------------------------------------------------------------------------------

-- * Located

-------------------------------------------------------------------------------

unL :: GenLocated l a -> a
unL (L _ x) = x

-------------------------------------------------------------------------------

-- * NamedThing instances

-------------------------------------------------------------------------------

instance NamedThing (TyClDecl GhcRn) where
  getName = tcdName

-------------------------------------------------------------------------------

-- * Subordinates

-------------------------------------------------------------------------------

class Parent a where
  children :: a -> [Name]

instance Parent (ConDecl GhcRn) where
  children con =
    case getRecConArgs_maybe con of
      Nothing -> []
      Just flds -> map (foExt . unLoc) $ concatMap (cd_fld_names . unLoc) (unLoc flds)

instance Parent (TyClDecl GhcRn) where
  children d
    | isDataDecl d =
        map unLoc $
          concatMap (getConNames . unLoc) $
            (dd_cons . tcdDataDefn) d
    | isClassDecl d =
        map (unLoc . fdLName . unLoc) (tcdATs d)
          ++ [unLoc n | L _ (TypeSig _ ns _) <- tcdSigs d, n <- ns]
    | otherwise = []

-- | A parent and its children
family :: (NamedThing a, Parent a) => a -> (Name, [Name])
family = getName &&& children

familyConDecl :: ConDecl GHC.GhcRn -> [(Name, [Name])]
familyConDecl d = zip (map unLoc (getConNames d)) (repeat $ children d)

{- | A mapping from the parent (main-binder) to its children and from each
child to its grand-children, recursively.
-}
families :: TyClDecl GhcRn -> [(Name, [Name])]
families d
  | isDataDecl d = family d : concatMap (familyConDecl . unLoc) (dd_cons (tcdDataDefn d))
  | isClassDecl d = [family d]
  | otherwise = []

-- | A mapping from child to parent
parentMap :: TyClDecl GhcRn -> [(Name, Name)]
parentMap d = [(c, p) | (p, cs) <- families d, c <- cs]

-- | The parents of a subordinate in a declaration
parents :: Name -> HsDecl GhcRn -> [Name]
parents n (TyClD _ d) = [p | (c, p) <- parentMap d, c == n]
parents _ _ = []

-------------------------------------------------------------------------------

-- * Free variables of a 'Type'

-------------------------------------------------------------------------------

{- | Get free type variables in a 'Type' in their order of appearance.
See [Ordering of implicit variables].
-}
orderedFVs ::
  VarSet
  -- ^ free variables to ignore
  -> [Type]
  -- ^ types to traverse (in order) looking for free variables
  -> [TyVar]
  -- ^ free type variables, in the order they appear in
orderedFVs vs tys =
  reverse . fst $ tyCoFVsOfTypes' tys (const True) vs ([], emptyVarSet)

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
tyCoFVsOfType' :: Type -> FV
tyCoFVsOfType' (TyVarTy v) a b c = (FV.unitFV v `unionFV` tyCoFVsOfType' (tyVarKind v)) a b c
tyCoFVsOfType' (TyConApp _ tys) a b c = tyCoFVsOfTypes' tys a b c
tyCoFVsOfType' (LitTy{}) a b c = emptyFV a b c
tyCoFVsOfType' (AppTy fun arg) a b c = (tyCoFVsOfType' arg `unionFV` tyCoFVsOfType' fun) a b c
tyCoFVsOfType' (FunTy _ w arg res) a b c =
  ( tyCoFVsOfType' w
      `unionFV` tyCoFVsOfType' res
      `unionFV` tyCoFVsOfType' arg
  )
    a
    b
    c
tyCoFVsOfType' (ForAllTy bndr ty) a b c = tyCoFVsBndr' bndr (tyCoFVsOfType' ty) a b c
tyCoFVsOfType' (CastTy ty _) a b c = (tyCoFVsOfType' ty) a b c
tyCoFVsOfType' (CoercionTy _) a b c = emptyFV a b c

{- | Just like 'tyCoFVsOfTypes', but traverses type variables in reverse order
of appearance.
-}
tyCoFVsOfTypes' :: [Type] -> FV
tyCoFVsOfTypes' (ty : tys) fv_cand in_scope acc = (tyCoFVsOfTypes' tys `unionFV` tyCoFVsOfType' ty) fv_cand in_scope acc
tyCoFVsOfTypes' [] fv_cand in_scope acc = emptyFV fv_cand in_scope acc

{- | Just like 'tyCoFVsBndr', but traverses type variables in reverse order of
appearance.
-}
tyCoFVsBndr' :: TyVarBinder -> FV -> FV
tyCoFVsBndr' (Bndr tv _) fvs = FV.delFV tv fvs `unionFV` tyCoFVsOfType' (tyVarKind tv)

-------------------------------------------------------------------------------

-- * Defaulting RuntimeRep variables

-------------------------------------------------------------------------------

{- | Traverses the type, defaulting type variables of kind 'RuntimeRep' to
'LiftedType'. See 'defaultRuntimeRepVars' in GHC.Iface.Type the original such
function working over `IfaceType`'s.
-}
defaultRuntimeRepVars :: Type -> Type
defaultRuntimeRepVars = go emptyVarEnv
 where
  go :: TyVarEnv () -> Type -> Type
  go subs (ForAllTy (Bndr var flg) ty)
    | isRuntimeRepVar var
    , isInvisibleArgFlag flg =
        let subs' = extendVarEnv subs var ()
         in go subs' ty
    | otherwise =
        ForAllTy
          (Bndr (updateTyVarKind (go subs) var) flg)
          (go subs ty)
  go subs (TyVarTy tv)
    | tv `elemVarEnv` subs =
        liftedRepTy
    | otherwise =
        TyVarTy (updateTyVarKind (go subs) tv)
  go subs (TyConApp tc tc_args) =
    TyConApp tc (map (go subs) tc_args)
  go subs (FunTy af w arg res) =
    FunTy af (go subs w) (go subs arg) (go subs res)
  go subs (AppTy t u) =
    AppTy (go subs t) (go subs u)
  go subs (CastTy x co) =
    CastTy (go subs x) co
  go _ ty@(LitTy{}) = ty
  go _ ty@(CoercionTy{}) = ty
