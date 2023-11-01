{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module Pollock.CompatGHC
  ( ClassMinimalDef
  , ConLike (..)
  , unpackFS
  , HscEnv (hsc_unit_env)
  , showSDoc
  , readIORef
  , HasCallStack
  , TcGblEnv
    ( tcg_exports
    , tcg_insts
    , tcg_fam_insts
    , tcg_src
    , tcg_warns
    , tcg_th_docs
    , tcg_mod
    , tcg_semantic_mod
    , tcg_rdr_env
    , tcg_doc_hdr
    , tcg_rn_imports
    , tcg_rn_decls
    , tcg_rn_exports
    )
  , finalSafeMode
  , getOccString
  , occNameString
  , getSrcSpan
  , isDataConName
  , isValName
  , nameIsLocalOrFrom
  , nameOccName
  , emptyOccEnv
  , PromotionFlag (..)
  , lookupNameEnv
  , GlobalRdrEnv
  , greMangledName
  , lookupGlobalRdrEnv
  , elemNameSet
  , mkNameSet
  , HscSource (..)
  , SourceText (..)
  , sl_fs
  , nonDetEltsUniqMap
  , ue_units
  , Warnings (..)
  , WarningTxt (..)
  , PackageName (..)
  , pprPanic
  , declTypeDocs
  , extractTHDocs
  , getInstLoc
  , getMainDeclBinder
  , isValD
  , nubByName
  , subordinates
  , topDecls
  -- GHC
  , DynFlags (language, extensionFlags)
  , GenLocated (L)
  , HsDocString
  , LexicalFixity (..)
  , Name
  , RdrName (Qual, Exact, Orig, Unqual)
  , SafeHaskellMode
  , SrcLoc (RealSrcLoc)
  , TyThing (..)
  , alphaTyVars
  , renderHsDocString
  , classSCTheta
  , classMinimalDef
  , classTvsFds
  -- GHC
  , GhcRn
  , IdP
  , Language
  , LHsDecl
  , Module
  , Sig (FixSig, SpecSig, InlineSig, ClassOpSig, MinimalSig, PatSynSig, TypeSig)
  , LHsSigType
  , LHsSigWcType
  , LHsType
  , HsType
    ( HsKindSig
    , HsParTy, HsAppKindTy
    , HsBangTy
    , HsTupleTy
    , HsSumTy
    , HsExplicitTupleTy
    , HsListTy
    , HsExplicitListTy
    , HsIParamTy
    , HsOpTy
    , HsTyVar
    , HsAppTy
    , HsFunTy
    , HsTyLit
    , HsQualTy
    , HsForAllTy
    , hst_ctxt
    , hst_xqual
    , hst_tele
    , hst_xforall
    , hst_body
    )
  , LHsTyVarBndr
  , LHsQTyVars (HsQTvs, hsq_explicit, hsq_ext)
  , LHsContext
  , HsArg (HsValArg, HsArgPar, HsTypeArg)
  , HsTyVarBndr (KindedTyVar, UserTyVar)
  , HsDecl (TyClD, SigD, InstD, DocD, DerivD, ValD)
  , TyClDecl
    ( ClassDecl
    , FamDecl
    , SynDecl
    , DataDecl
    , tcdDExt
    , tcdCtxt
    , tcdFDs
    , tcdSigs
    , tcdMeths
    , tcdATs
    , tcdATDefs
    , tcdDocs
    , tcdCExt
    , tcdSExt
    , tcdRhs
    , tcdLName
    , tcdTyVars
    , tcdFixity
    , tcdDataDefn
    )
  , HsDataDefn
    ( HsDataDefn
    , dd_derivs
    , dd_ext
    , dd_ND
    , dd_ctxt
    , dd_cType
    , dd_kindSig
    , dd_cons
    )
  , LFamilyDecl
  , NewOrData (DataType, NewType)
  , LConDecl
  , ConDecl
    ( ConDeclGADT
    , ConDeclH98
    , con_doc
    , con_g_ext
    , con_names
    , con_bndrs
    , con_g_args
    , con_res_ty
    , con_ext
    , con_name
    , con_forall
    , con_ex_tvs
    , con_mb_cxt
    , con_args
    )
  , HsConDeclH98Details
  , ConDeclField (ConDeclField, cd_fld_type, cd_fld_names)
  , HsConDetails (InfixCon, RecCon, PrefixCon)
  , HsConDeclGADTDetails (..)
  , FieldOcc (foExt, FieldOcc)
  , FamilyDecl
    ( FamilyDecl
    , fdInjectivityAnn
    , fdExt
    , fdInfo
    , fdTopLevel
    , fdLName
    , fdTyVars
    , fdFixity
    , fdResultSig
    )
  , noAnn
  , noExtField
  , mkEmptyWildCardBndrs
  , noLocA
  , hsUnrestricted
  , noHsTok
  , noHsUniTok
  , mkHsForAllInvisTele
  , mkHsForAllVisTele
  , na2la
  , noAnnSrcSpan
  , isHsKindedTyVar
  , noTypeArgs
  , LocatedN
  , DataDeclRn (DataDeclRn)
  , FamEqn
    ( FamEqn
    , feqn_rhs
    , feqn_ext
    , feqn_tycon
    , feqn_bndrs
    , feqn_pats
    , feqn_fixity
    )
  , FamilyInfo (DataFamily, OpenTypeFamily, ClosedTypeFamily)
  , FamilyResultSig (TyVarSig, NoSig, KindSig)
  , FunDep (FunDep)
  , InjectivityAnn (InjectivityAnn)
  , InstDecl (TyFamInstD, DataFamInstD, ClsInstD)
  , LFamilyResultSig
  , LInjectivityAnn
  , LTyFamDefltDecl
  , TyFamDefltDecl
  , TyFamInstDecl (TyFamInstDecl, tfid_eqn, tfid_xtn)
  , TyFamInstEqn
  , HsArrow (..)
  , HsIPName (HsIPName)
  , HsLinearArrowTokens (HsPct1)
  , HsOuterTyVarBndrs
    ( HsOuterImplicit
    , HsOuterExplicit
    , hso_bndrs
    , hso_ximplicit
    , hso_xexplicit
    )
  , HsTupleSort (HsUnboxedTuple, HsBoxedOrConstraintTuple)
  , HsTyLit (..)
  , LHsKind
  , GhcPass
  , LSig
  , FixitySig (FixitySig)
  , HsSigType (HsSig, sig_body, sig_ext, sig_bndrs)
  , HsForAllTelescope (HsForAllInvis, hsf_invis_bndrs)
  , LHsTypeArg
  , LConDeclField
  , nlHsTyConApp
  , hsLTyVarName
  , hsQTvExplicit
  , isDataDecl
  , isClassDecl
  , getConNames
  ,ModuleName,
      SrcSpanAnnA,
      getRecConArgs_maybe,
      getBangType,
      collectHsBindBinders,
      getLocA,
      nameModule,
      tyClDeclTyVars,
      hsScaledThing,
      ExtractedTHDocs(ExtractedTHDocs, ethd_mod_header),
      HsDoc,
      WithHsDocIdentifiers(hsDocString),
      IE(IEModuleContents, IEGroup, IEDoc, IEDocNamed),
      ImportDecl(ideclName, ideclAs, ideclHiding),
      CollectFlag(CollNoDictBinders),
      SrcSpanAnn'(SrcSpanAnn, locA),
      RealSrcSpan,
      SrcSpan(..),
      ModSummary(ms_hspp_opts),
      ClsInstDecl(ClsInstDecl, cid_datafam_insts),
      DataFamInstDecl(DataFamInstDecl),
      DocDecl(DocCommentNamed, DocGroup)

  -- GHC.Plugins
  , GlobalRdrElt
  , isRuntimeRepVar
  , liftedRepTy
  , combineSrcSpans
  , elemVarEnv
  , emptyVarEnv
  , extendVarEnv
  , fsLit
  , gresToAvailInfo
  , isDataOcc
  , isTyConName
  , isVarOcc
  , languageExtensions
  , lookupGRE_RdrName
  , lookupSrcSpan
  , mkRealSrcLoc
  , nameSrcLoc
  , pprNameDefnLoc
  , rdrNameOcc
  , showPpr
  , tcName
  , Specificity (InferredSpec)
  , NamedThing (..)
  , VarSet
  , Type
    ( FunTy
    , AppTy
    , LitTy
    , CoercionTy
    , TyConApp
    , TyVarTy
    , CastTy
    , ForAllTy
    )
  , TyVar
  , TyVarBinder
  , TyVarEnv
  , TyCon
    ( tyConArity
    , tyConKind
    , famTcResVar
    , tyConTyVars
    , tyConResKind
    )
  , AnonArgFlag (InvisArg, VisArg)
  , TyCoVarBinder
  , ArgFlag (Required, Invisible)
  , VarBndr (..)
  , isInvisibleArgFlag
  , tyVarKind
  , emptyVarSet
  , unLoc
  , dataConFieldLabels
  , dataConIsInfix
  , dataConSrcBangs
  , dataConWrapperType
  , isFunTyCon
  , isNewTyCon
  , isOpenTypeFamilyTyCon
  , isPrimTyCon
  , synTyConRhs_maybe
  , tyConClass_maybe
  , tyConDataCons
  , srcLocSpan
  , DataCon
  , HsSrcBang (HsSrcBang)
  , SrcStrictness (NoSrcStrict)
  , SrcUnpackedness (NoSrcUnpack)
  , Kind
  , Mult
  , PredType
  , ThetaType
  , Id
  , eqTyConName
  , liftedTypeKindTyConName
  , listTyConName
  , promotedConsDataCon
  , promotedNilDataCon
  , unitTy
  , mkTyConApp
  , Var (varType)
  , TupleSort (UnboxedTuple, BoxedTuple, ConstraintTuple)
  , isVisibleBinder
  , mkVisFunTys
  , binderVars
  , isVisibleArgFlag
  , TyCoBinder
  , InvisTVBinder
  , dataConFullSig
  , dataConSourceArity
  , dataConUserTyVarBinders
  , isTupleDataCon
  , noFreeVarsOfType
  , tyCoVarsOfType
  , famTyConFlav_maybe
  , isClosedSynFamilyTyConWithAxiom_maybe
  , isGadtSyntaxTyCon
  , isPromotedDataCon
  , isPromotedDataCon_maybe
  , isUnboxedSumTyCon
  , tyConInjectivityInfo
  , tyConStupidTheta
  , tyConTuple_maybe
  , tyConVisibleTyVars
  , pattern Many
  , pattern One
  , appTyArgFlags
  , eqTypes
  , filterOutInvisibleTypes
  , isCoercionTy
  , isLiftedTypeKind
  , isStrLitTy
  , splitAppTys
  , splitFunTys
  , splitInvisPiTys
  , splitPiTys
  , tyCoBinderType
  , tyConAppNeedsKindSig
  , tyConAppTyCon_maybe
  , typeKind
  , getSrcLoc
  , isSymOcc
  , mkVarUnqual
  , emptyNameSet
  , mapLoc
  , isTyVar
  , mkTyCoVarBinder
  , tyVarName
  , elemVarSet
  , filterVarSet
  , isEmptyVarSet
  , mkVarSet
  , unionVarSet
  , unionVarSets
  , unitVarSet
  , equalLength
  , filterByList
  , filterOut
  , FamTyConFlav
    ( DataFamilyTyCon
    , OpenSynFamilyTyCon
    , ClosedSynFamilyTyCon
    , BuiltInSynFamTyCon
    , AbstractClosedSynFamilyTyCon
    )
  , Injectivity (..)
  , DefMethSpec (GenericDM)
  , TopLevelFlag (TopLevel)
  , FieldLabel (flLabel, flSelector)
  , ReqTVBinder
  , Uniquable (getUnique)
  , updateTyVarKind
  -- GHC.Builtin.Names
  , hasKey
  , boxedRepDataConKey
  , eqTyConKey
  , ipClassKey
  , liftedDataConKey
  , tYPETyConKey
  -- GHC.Core.Class
  , Class (classTyCon)
  , classATItems
  , classOpItems
  , ClassATItem (..)
  , ClassOpItem
  -- GHC.Core.Coercion.Axiom
  , CoAxiom (CoAxiom, co_ax_branches, co_ax_tc)
  , CoAxBranch (CoAxBranch, cab_rhs, cab_tvs, cab_lhs)
  , coAxiomSingleBranch_maybe
  , fromBranches
  -- GHC.Core.PatSyn
  , PatSyn
  , patSynSigBndr
  -- GHC.Core.TyCo.Rep
  , scaledThing
  , TyLit (..)
  -- GHC.Data.Bag
  , emptyBag
  -- GHC.Data.StringBuffer
  , stringToStringBuffer
  -- GHC.Driver.Config.Parser
  , initParserOpts
  -- GHC.HsToCore.Docs
  , sigNameNoLoc
  --  GHC.Parser
  , parseIdentifier
  --  GHC.Parser.Lexer
  , ParseResult (PFailed, POk)
  , initParserState
  , unP
  -- GHC.Parser.PostProcess
  , setRdrNameSpace
  -- GHC.Types.Avail
  , AvailInfo
  , Avails
  , availExportsDecl
  , availName
  , availNamesWithSelectors
  , availSubordinateGreNames
  , availsToNameEnv
  , greNameMangledName
  , nubAvails
  -- GHC.Utils.FV
  , FV
  , delFV
  , emptyFV
  , unionFV
  , unitFV
  -- GHC.Utils.Outputable
  , Outputable
  -- GHC.Utils.Panic.Plain
  , assert
  -- ghc-boot GHC.LanguageExtensions
  , Extension

  -- compatability shims defined here
  , availFromName
  -- helpers defined here
  , enumSetToList
  ) where

import GHC hiding (typeKind)
import GHC.Plugins hiding (delFV, unitFV)

import GHC.Builtin.Names
  ( boxedRepDataConKey
  , eqTyConKey
  , hasKey
  , ipClassKey
  , liftedDataConKey
  , tYPETyConKey
  )

import GHC.Core.Class (Class (classTyCon), ClassATItem (..), ClassMinimalDef, ClassOpItem, classATItems, classMinimalDef, classOpItems)

import GHC.Core.Coercion.Axiom
  ( CoAxBranch (CoAxBranch, cab_lhs, cab_rhs, cab_tvs)
  , CoAxiom (CoAxiom, co_ax_branches, co_ax_tc)
  , coAxiomSingleBranch_maybe
  , fromBranches
  )
import GHC.Core.ConLike (ConLike (..))
import GHC.Core.PatSyn (PatSyn, patSynSigBndr)
import GHC.Core.TyCo.Rep
  ( TyLit (..)
  , Type
    ( AppTy
    , CastTy
    , CoercionTy
    , ForAllTy
    , FunTy
    , LitTy
    , TyConApp
    , TyVarTy
    )
  , scaledThing
  )
import GHC.Data.Bag (emptyBag)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.HsToCore.Docs
  ( declTypeDocs
  , extractTHDocs
  , getInstLoc
  , getMainDeclBinder
  , isValD
  , nubByName
  , sigNameNoLoc
  , subordinates
  , topDecls
  )
import GHC.IORef (readIORef)
import GHC.Parser (parseIdentifier)
import GHC.Parser.Lexer (ParseResult (PFailed, POk), initParserState, unP)
import GHC.Parser.PostProcess (setRdrNameSpace)
import GHC.Tc.Types
  ( TcGblEnv
      ( tcg_doc_hdr
      , tcg_exports
      , tcg_fam_insts
      , tcg_insts
      , tcg_mod
      , tcg_rdr_env
      , tcg_rn_decls
      , tcg_rn_exports
      , tcg_rn_imports
      , tcg_semantic_mod
      , tcg_src
      , tcg_th_docs
      , tcg_warns
      )
  )
import GHC.Tc.Utils.Monad (finalSafeMode)
import GHC.Types.SourceFile (HscSource (..))
import GHC.Types.SourceText (SourceText (..), sl_fs)
import GHC.Types.Unique.Map (nonDetEltsUniqMap)
import GHC.Unit.Env (ue_units)
import GHC.Unit.Module.Warnings (WarningTxt (..), Warnings (..))
import GHC.Utils.FV (FV, delFV, emptyFV, unionFV, unitFV)
import GHC.Utils.Panic.Plain (assert)


#if __GLASGOW_HASKELL__ == 908
import GHC.Types.Avail
  ( AvailInfo
  , Avails
  , availExportsDecl
  , availName
  , availNamesWithSelectors
  , availSubordinateGreNames
  , availsToNameEnv
  , greNameMangledName
  , nubAvails
  )
#elif __GLASGOW_HASKELL__ == 906
#elif __GLASGOW_HASKELL__ == 904
import GHC.Types.Avail
  ( AvailInfo
  , Avails
  , avail
  , availExportsDecl
  , availName
  , availNamesWithSelectors
  , availSubordinateGreNames
  , availsToNameEnv
  , greNameMangledName
  , nubAvails
  )
#endif

-- ghc-boot
import GHC.LanguageExtensions (Extension)

#if __GLASGOW_HASKELL__ == 908

-- | availFromName is simply a constructor in GHC9.8, but in previous versions it was a real
-- function so we keep it as such for compat
availFromName :: Name -> AvailInfo
availFromName = Avail

#elif __GLASGOW_HASKELL__ == 906
#elif __GLASGOW_HASKELL__ == 904

-- | availFromName is simply a constructor in GHC9.8, but in previous versions it was a real
-- function so we keep it as such for compat
availFromName :: Name -> AvailInfo
availFromName = avail


#endif

-- | A renamed toList to avoid clashing with the Foldable version
enumSetToList :: (Enum a) => EnumSet.EnumSet a -> [a]
enumSetToList = EnumSet.toList
