{-# LANGUAGE CPP #-}

module Pollock.CompatGHC
  ( readIORef
  , TcGblEnv
    ( tcg_exports
    , tcg_insts
    , tcg_fam_insts
    , tcg_warns
    , tcg_th_docs
    , tcg_semantic_mod
    , tcg_rdr_env
    , tcg_rn_imports
    , tcg_rn_decls
    , tcg_rn_exports
    )
  , getSrcSpan
  , nameIsLocalOrFrom
  , emptyOccEnv
  , lookupNameEnv
  , GlobalRdrEnv
  , Warnings
  , WarningTxt
  , declTypeDocs
  , extractTHDocs
  , getInstLoc
  , getMainDeclBinder
  , isValD
  , nubByName
  , subordinates
  , topDecls
  -- GHC
  , DynFlags (generalFlags)
  , GenLocated (L)
  , HsDocString
  , Name
  , renderHsDocString
  , GhcRn
  , IdP
  , LHsDecl
  , Module
  , HsDecl (InstD, DerivD, ValD, SigD, DocD)
  , feqn_tycon
  , InstDecl
    ( TyFamInstD
    )
  , TyFamInstDecl (TyFamInstDecl)
  , ModuleName
  , SrcSpanAnnA
  , collectHsBindBinders
  , getLocA
  , nameModule
  , ExtractedTHDocs (ExtractedTHDocs, ethd_mod_header)
  , HsDoc
  , WithHsDocIdentifiers (hsDocString)
  , IE (IEModuleContents, IEGroup, IEDoc, IEDocNamed)
  , ImportDecl
  , ideclName
  , ideclAs
  , ideclImportList
  , ImportListInterpretation (Exactly, EverythingBut)
  , CollectFlag (CollNoDictBinders)
  , RealSrcSpan
  , SrcSpan (RealSrcSpan)
  , DocDecl (DocCommentNamed, DocGroup)
  , Located
  , HscEnv (hsc_dflags)
  -- GHC.Plugins
  , GeneralFlag (Opt_Haddock)
  , lookupSrcSpan
  , getName
  , unLoc
  -- GHC.Types.Avail
  , AvailInfo
  , Avails
  , availExportsDecl
  , availName
  , availNames
  , availSubordinateNames
  , availsToNameEnv
  , nubAvails
  -- GHC.Types.SourceText
  , StringLiteral
  -- compatability shims defined here
  , processWarnSome
  , mapWarningTxtMsg
  , getHeaderInfo
  -- helpers defined here
  , nonDetEltUniqMapToMap
  , insertEnumSet
  , stringLiteralToString
  ) where

-- The following are library and non-reexorted imports
import qualified Control.Arrow as Arrow
import qualified Control.Monad as M
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Types.Unique.Map as UniqMap
import qualified GHC.Tc.Types as TcTypes

import GHC
  ( CollectFlag (CollNoDictBinders)
  , DocDecl (DocCommentNamed, DocGroup)
  , DynFlags (generalFlags)
  , ExtractedTHDocs (ExtractedTHDocs, ethd_mod_header)
  , FamEqn (feqn_tycon)
  , GenLocated (L)
  , GeneralFlag (Opt_Haddock)
  , GhcRn
  , HsDecl (DerivD, DocD, InstD, SigD, ValD)
  , HsDoc
  , HsDocString
  , HscEnv
  , IE (IEDoc, IEDocNamed, IEGroup, IEModuleContents)
  , IdP
  , InstDecl (TyFamInstD)
  , LHsDecl
  , Located
  , Module
  , ModuleName
  , Name
  , NamedThing (getName)
  , RealSrcSpan
  , SrcSpan (RealSrcSpan)
  , SrcSpanAnnA
  , TyFamInstDecl (TyFamInstDecl)
  , WithHsDocIdentifiers (hsDocString)
  , collectHsBindBinders
  , getLocA
  , nameModule
  , renderHsDocString
  , unLoc
  )
import GHC.HsToCore.Docs
  ( declTypeDocs
  , extractTHDocs
  , getInstLoc
  , getMainDeclBinder
  , isValD
  , nubByName
  , subordinates
  , topDecls
  )
import GHC.IORef (readIORef)
import GHC.Plugins
  ( GlobalRdrElt
  , GlobalRdrEnv
  , HscEnv (hsc_dflags)
  , OccEnv
  , OccName
  , emptyOccEnv
  , getSrcSpan
  , lookupNameEnv
  , lookupOccEnv
  , lookupSrcSpan
  , nameIsLocalOrFrom
  , unpackFS
  )
import GHC.Tc.Types
  ( TcGblEnv
      ( tcg_exports
      , tcg_fam_insts
      , tcg_insts
      , tcg_rdr_env
      , tcg_rn_decls
      , tcg_rn_exports
      , tcg_rn_imports
      , tcg_semantic_mod
      , tcg_th_docs
      , tcg_warns
      )
  )
import GHC.Types.SourceText (StringLiteral, sl_fs)
import GHC.Unit.Module.Warnings (WarningTxt (DeprecatedTxt, WarningTxt), Warnings (WarnSome))

#if __GLASGOW_HASKELL__ == 910
import GHC (ImportDecl, ideclImportList, ideclAs, ideclName, ImportListInterpretation (Exactly, EverythingBut))
import GHC.Plugins (GlobalRdrEltX, greName)
import qualified GHC.Parser.Annotation as Annotation
import GHC.Types.Avail
  ( AvailInfo
  , Avails
  , availExportsDecl
  , availName
  , availNames
  , availSubordinateNames
  , availsToNameEnv
  , nubAvails
  )
import GHC.Types.Unique.Map (nonDetUniqMapToList)

#elif __GLASGOW_HASKELL__ == 908
import GHC (ImportDecl, ideclImportList, ideclAs, ideclName, ImportListInterpretation (Exactly, EverythingBut))
import GHC.Plugins (GlobalRdrEltX, greName)
import GHC.Types.Avail
  ( AvailInfo
  , Avails
  , availExportsDecl
  , availName
  , availNames
  , availSubordinateNames
  , availsToNameEnv
  , nubAvails
  )
import GHC.Types.Unique.Map (nonDetUniqMapToList)

#elif __GLASGOW_HASKELL__ == 906
import GHC (ImportDecl, ideclImportList, ideclAs, ideclName,  ImportListInterpretation (Exactly, EverythingBut))
import GHC.Plugins (greMangledName)
import GHC.Types.Avail
  ( AvailInfo
  , Avails
  , availExportsDecl
  , availName
  , availNames
  , availSubordinateGreNames
  , availsToNameEnv
  , greNameMangledName
  , nubAvails
  )

#elif __GLASGOW_HASKELL__ == 904
import GHC (ImportDecl(ideclHiding, ideclAs, ideclName),       XRec)
import GHC.Plugins (greMangledName)
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
#endif

#if __GLASGOW_HASKELL__ == 910
-- | Compatibility helper to ease development against multiple version
getHeaderInfo :: TcGblEnv -> Maybe HsDocString
getHeaderInfo = fmap (hsDocString . unLoc) . fst . TcTypes.tcg_hdr_info

lookupOccName :: OccEnv [GlobalRdrEltX info] -> OccName -> [Name]
lookupOccName env = fmap greName . lookupOcc env

processWarnSome :: Warnings pass -> OccEnv [GlobalRdrElt] -> [Name] -> [(Name, WarningTxt pass)]
processWarnSome warnings gre names =
  case warnings of
    WarnSome ws exports ->
      let
        keepByName :: [(Name,b)] -> [(Name,b)]
        keepByName = filter (\x -> (fst x) `elem` names)

        keepOnlyKnownNameWarnings = keepByName . mappend exports . M.join . fmap (explodeSnd . Arrow.first (lookupOccName gre))

        explodeSnd :: Functor f => (f a, b) -> f (a, b)
        explodeSnd (as,b) = fmap ((flip (,) b)) as
      in
        keepOnlyKnownNameWarnings ws
    _ ->
      -- Note we want to catch all here so we can limit imports that vary for different GHC versions.
      mempty

-- | Compatability helper to let us get at the deprecated and warning messages consistently
mapWarningTxtMsg :: ([Annotation.LocatedE
                       (WithHsDocIdentifiers StringLiteral pass)] -> t)
                 -> ([Annotation.LocatedE
                      (WithHsDocIdentifiers StringLiteral pass)] -> t)
                 -> WarningTxt pass
                 -> t
mapWarningTxtMsg deprecatedFn warnFn warnTxt =
  case warnTxt of
    DeprecatedTxt _ msgs -> deprecatedFn msgs
    WarningTxt _ _ msgs -> warnFn msgs

#elif __GLASGOW_HASKELL__ == 908
-- | Helper for using the GHC 9.10 api
getHeaderInfo :: TcGblEnv -> Maybe HsDocString
getHeaderInfo = fmap (hsDocString . unLoc) . TcTypes.tcg_doc_hdr

lookupOccName :: OccEnv [GlobalRdrEltX info] -> OccName -> [Name]
lookupOccName env = fmap greName . lookupOcc env

processWarnSome :: Warnings pass -> OccEnv [GlobalRdrElt] -> [Name] -> [(Name, WarningTxt pass)]
processWarnSome warnings gre names =
  case warnings of
    WarnSome ws exports ->
      let
        keepByName :: [(Name,b)] -> [(Name,b)]
        keepByName = filter (\x -> (fst x) `elem` names)

        keepOnlyKnownNameWarnings = keepByName . mappend exports . M.join . fmap (explodeSnd . Arrow.first (lookupOccName gre))

        explodeSnd :: Functor f => (f a, b) -> f (a, b)
        explodeSnd (as,b) = fmap ((flip (,) b)) as
      in
        keepOnlyKnownNameWarnings ws
    _ ->
      -- Note we want to catch all here so we can limit imports that vary for different GHC versions.
      mempty

-- | Compatability helper to let us get at the deprecated and warning messages consistently
mapWarningTxtMsg ::
  ([Located (WithHsDocIdentifiers StringLiteral pass)] -> t)
  -> ([Located (WithHsDocIdentifiers StringLiteral pass)] -> t)
  -> WarningTxt pass
  -> t
mapWarningTxtMsg deprecatedFn warnFn warnTxt =
  case warnTxt of
    DeprecatedTxt _ msgs -> deprecatedFn msgs
    WarningTxt _ _ msgs -> warnFn msgs

#elif __GLASGOW_HASKELL__ == 906
-- | Helper for using the GHC 9.10 api
getHeaderInfo :: TcGblEnv -> Maybe HsDocString
getHeaderInfo = fmap (hsDocString . unLoc) . TcTypes.tcg_doc_hdr

-- | Shim for using the GHC 9.8+ api
availSubordinateNames :: AvailInfo -> [Name]
availSubordinateNames = fmap greNameMangledName . availSubordinateGreNames

lookupOccName :: OccEnv [GlobalRdrElt] -> OccName -> [Name]
lookupOccName env = fmap greMangledName . lookupOcc env

processWarnSome :: Warnings pass -> OccEnv [GlobalRdrElt] -> [Name] -> [(Name, WarningTxt pass)]
processWarnSome warnings gre names =
  case warnings of
    WarnSome ws ->
      let
        keepByName :: [(Name,b)] -> [(Name,b)]
        keepByName = filter (\x -> (fst x) `elem` names)

        keepOnlyKnownNameWarnings :: [(OccName, b)] -> [(Name, b)]
        keepOnlyKnownNameWarnings = keepByName . M.join . fmap (explodeSnd . Arrow.first (lookupOccName gre))

        explodeSnd :: Functor f => (f a, b) -> f (a, b)
        explodeSnd (as,b) = fmap ((flip (,) b)) as
      in
        keepOnlyKnownNameWarnings ws
    _ ->
      -- Note we want to catch all here so we can limit imports that vary for different GHC versions.
      mempty

-- | Shim for using the GHC 9.8+ api, note the previous name was somewhat confusing as it does result
-- in a list not a map!
nonDetUniqMapToList :: UniqMap.UniqMap k a -> [(k, a)]
nonDetUniqMapToList = UniqMap.nonDetEltsUniqMap

-- | Compatability helper to let us get at the deprecated and warning messages consistently
mapWarningTxtMsg ::
  ([Located (WithHsDocIdentifiers StringLiteral pass)] -> t)
  -> ([Located (WithHsDocIdentifiers StringLiteral pass)] -> t)
  -> WarningTxt pass
  -> t
mapWarningTxtMsg deprecatedFn warnFn warnTxt =
  case warnTxt of
    DeprecatedTxt _ msgs -> deprecatedFn msgs
    WarningTxt _ msgs -> warnFn msgs

#elif __GLASGOW_HASKELL__ == 904
-- | Helper for using the GHC 9.10 api
getHeaderInfo :: TcGblEnv -> Maybe HsDocString
getHeaderInfo = fmap (hsDocString . unLoc) . TcTypes.tcg_doc_hdr

-- | Compatibility shim as this datatype was added in GHC 9.6, along with 'ideclImportList'
data ImportListInterpretation = Exactly | EverythingBut

-- | Compatibility shim as GHC 9.4 used 'ideclHiding', but later changed to 'ideclImportList'.
ideclImportList :: ImportDecl pass -> Maybe (ImportListInterpretation, XRec pass [XRec pass (IE pass)])
ideclImportList idecl =
  case ideclHiding idecl of
    Nothing -> Nothing
    Just (True, n) -> Just (EverythingBut,n)
    Just (False,n) -> Just (Exactly,n)

-- | availNames was changed to include the selectors, it would seem, so we create a shim for 9.4 to
-- have an api more like later versions
availNames :: AvailInfo -> [Name]
availNames = availNamesWithSelectors

-- | Shim for using the GHC 9.8+ api
availSubordinateNames :: AvailInfo -> [Name]
availSubordinateNames = fmap greNameMangledName . availSubordinateGreNames

lookupOccName :: OccEnv [GlobalRdrElt] -> OccName -> [Name]
lookupOccName env = fmap greMangledName . lookupOcc env

processWarnSome :: Warnings pass -> OccEnv [GlobalRdrElt] -> [Name] -> [(Name, WarningTxt pass)]
processWarnSome warnings gre names =
  case warnings of
    WarnSome ws ->
      let
        keepByName :: [(Name,b)] -> [(Name,b)]
        keepByName = filter (\x -> (fst x) `elem` names)

        keepOnlyKnownNameWarnings :: [(OccName, b)] -> [(Name, b)]
        keepOnlyKnownNameWarnings = keepByName . M.join . fmap (explodeSnd . Arrow.first (lookupOccName gre))

        explodeSnd :: Functor f => (f a, b) -> f (a, b)
        explodeSnd (as,b) = fmap ((flip (,) b)) as
      in
        keepOnlyKnownNameWarnings ws
    _ ->
      -- Note we want to catch all here so we can limit imports that vary for different GHC versions.
      mempty

-- | Shim for using the GHC 9.8+ api, note the previous name was somewhat confusing as it does result
-- in a list not a map!
nonDetUniqMapToList :: UniqMap.UniqMap k a -> [(k, a)]
nonDetUniqMapToList = UniqMap.nonDetEltsUniqMap

-- | Compatability helper to let us get at the deprecated and warning messages consistently
mapWarningTxtMsg ::
  ([Located (WithHsDocIdentifiers StringLiteral pass)] -> t)
  -> ([Located (WithHsDocIdentifiers StringLiteral pass)] -> t)
  -> WarningTxt pass
  -> t
mapWarningTxtMsg deprecatedFn warnFn warnTxt =
  case warnTxt of
    DeprecatedTxt _ msgs -> deprecatedFn msgs
    WarningTxt _ msgs -> warnFn msgs

#endif

-- | Simple helper used above but definable consistently across GHC versions.
lookupOcc :: OccEnv [a] -> OccName -> [a]
lookupOcc env =
  Maybe.fromMaybe mempty . lookupOccEnv env

-- | Helper to convert from UniqMap to Map in a consistent fashion.
nonDetEltUniqMapToMap :: (Ord k) => UniqMap.UniqMap k a -> Map.Map k a
nonDetEltUniqMapToMap = Map.fromList . nonDetUniqMapToList

-- | A Helper to keep the interface clean avoiding any potential conflicts with 'insert'.
insertEnumSet :: (Enum a) => a -> EnumSet.EnumSet a -> EnumSet.EnumSet a
insertEnumSet = EnumSet.insert

stringLiteralToString :: StringLiteral -> String
stringLiteralToString = unpackFS . sl_fs
