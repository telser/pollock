{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wwarn #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

{- |
Module      :  Haddock.Interface.Create
Copyright   :  (c) Simon Marlow      2003-2006,
                   David Waern       2006-2009,
                   Mateusz Kowalczyk 2013
License     :  BSD-like

Maintainer  :  haddock@projects.haskell.org
Stability   :  experimental
Portability :  portable

This module provides a single function 'createInterface',
which creates a Haddock 'Interface' from the typechecking
results 'TypecheckedModule' from GHC.
-}
module Haddock.Interface.Coverage (IfM, runIfM, pollock_getCoverage) where

import Haddock.Convert (PrintRuntimeReps (..), tyThingToLHsDecl)
import Haddock.Doc (metaDocAppend)
import Haddock.GhcUtils
  ( addClassContext
  , filterSigNames
  , lHsQTyVarsToTypes
  , mkEmptySigType
  , moduleString
  , parents
  , pretty
  , restrictTo
  , sigName
  , unL
  )
import Haddock.Interface.LexParseRn
  ( processDocString
  , processDocStringFromString
  , processDocStringParas
  , processDocStrings
  , processModuleHeader
  )
import Haddock.Options (modulePackageInfo)
import Haddock.Types

import qualified Control.Applicative as Applicative
import Control.Monad (liftM)
import qualified Control.Monad.IO.Class as MIO
import Control.Monad.Reader (MonadReader (..), ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Control.Monad.Writer (MonadWriter (..))
import Data.Bitraversable (bitraverse)
import Data.Foldable (traverse_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (find, foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, maybeToList)
import qualified Data.String as String
import Data.Traversable (for)

import CompatGHC

import GHC hiding (lookupName)
import GHC.Types.Avail hiding (avail)
import qualified GHC.Types.Avail as Avail
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Outputable as O

newtype IfEnv m = IfEnv
  { ife_lookup_name :: Name -> m (Maybe TyThing)
  -- ^ Lookup names in the environment.
  }

{- | A monad in which we create Haddock interfaces. Not to be confused with
`GHC.Tc.Types.IfM` which is used to write GHC interfaces.

In the past `createInterface` was running in the `Ghc` monad but proved hard
to sustain as soon as we moved over for Haddock to be a plugin. Also abstracting
over the Ghc specific clarifies where side effects happen.
-}
newtype IfM m a = IfM {unIfM :: ReaderT (IfEnv m) (WriterT ErrorMessages m) a}

deriving newtype instance (Functor m) => Functor (IfM m)
deriving newtype instance (Monad m) => Applicative.Applicative (IfM m)
deriving newtype instance (Monad m) => Monad (IfM m)
deriving newtype instance (MIO.MonadIO m) => MIO.MonadIO (IfM m)
deriving newtype instance (Monad m) => MonadReader (IfEnv m) (IfM m)
deriving newtype instance (Monad m) => MonadWriter ErrorMessages (IfM m)
deriving newtype instance (Monad m) => ReportErrorMessage (IfM m)

-- | Run an `IfM` action.
runIfM ::
  -- \| Lookup a global name in the current session. Used in cases
  -- where declarations don't
  (Functor m) =>
  (Name -> m (Maybe TyThing))
  -> IfM m a
  -- ^ The action to run.
  -> m (a, [ErrMsg])
  -- ^ Result and accumulated error/warning messages.
runIfM lookup_name action = do
  let
    if_env =
      IfEnv
        { ife_lookup_name = lookup_name
        }
  fmap errorMessagesToList Applicative.<$> runWriterT (runReaderT (unIfM action) if_env)

liftErrMsg :: (Monad m) => ErrMsgM a -> IfM m a
liftErrMsg action = do
  IfM (writer (runErrMsgM action))

lookupName :: (Monad m) => Name -> IfM m (Maybe TyThing)
lookupName name = IfM $ do
  lookup_name <- asks ife_lookup_name
  lift $ lift (lookup_name name)

pollock_getCoverage ::
  (MIO.MonadIO m) =>
  TcGblEnv
  -> HscEnv
  -> ModSummary
  -> IfM m (Int, Int)
pollock_getCoverage tcGblEnv hscEnv modSummary = do
  let
    dflags = ms_hspp_opts modSummary
    unitState = ue_units $ hsc_unit_env hscEnv
    (pkg_name_fs, _) =
      modulePackageInfo unitState (Just (tcg_mod tcGblEnv))

    pkg_name :: Maybe Package
    pkg_name =
      let
        unpack (PackageName name) = unpackFS name
       in
        fmap unpack pkg_name_fs

    -- TyThings that have instances defined in this module
    local_instances :: [Name]
    local_instances =
      [ name
      | name <- map getName (tcg_insts tcGblEnv) ++ map getName (tcg_fam_insts tcGblEnv)
      , nameIsLocalOrFrom (tcg_semantic_mod tcGblEnv) name
      ]

    is_sig = (tcg_src tcGblEnv) == HsigFile
    exportedNames = exported_names tcGblEnv
  -- Warnings on declarations in this module
  decl_warnings <-
    liftErrMsg (mkWarningMap dflags (tcg_warns tcGblEnv) (tcg_rdr_env tcGblEnv) exportedNames)

  ds <- decls tcGblEnv
  -- Infer module safety
  safety <- MIO.liftIO (finalSafeMode dflags tcGblEnv)

  -- The docs added via Template Haskell's putDoc
  thDocs@ExtractedTHDocs{ethd_mod_header = thMbDocStr} <-
    MIO.liftIO . fmap extractTHDocs . readIORef $ tcg_th_docs tcGblEnv

  -- Process the top-level module header documentation.
  -- TODO-POL This should be exposed out
  (!_info, _header_doc) <-
    liftErrMsg $
      processModuleHeader
        dflags
        pkg_name
        (tcg_rdr_env tcGblEnv)
        safety
        ( fmap hsDocString thMbDocStr
            Applicative.<|> (hsDocString . unLoc Applicative.<$> (tcg_doc_hdr tcGblEnv))
        )

  maps <- liftErrMsg (pollock_mkMaps dflags pkg_name (tcg_rdr_env tcGblEnv) local_instances ds thDocs)

  export_items <-
    pollock_mkExportItems
      is_sig
      pkg_name
      (tcg_mod tcGblEnv)
      (tcg_semantic_mod tcGblEnv)
      decl_warnings
      (tcg_rdr_env tcGblEnv)
      exportedNames
      (fmap fst ds)
      maps
      (imported_modules tcGblEnv)
      (export_list tcGblEnv)
      (tcg_exports tcGblEnv)
      dflags
  let
    pruned_export_items = pruneExportItems export_items
    !haddockable = 1 + length export_items -- module + exports
    !haddocked = (if isJust (tcg_doc_hdr tcGblEnv) then 1 else 0) + length pruned_export_items

  pure (haddockable, haddocked)

-- Module imports of the form `import X`. Note that there is
-- a) no qualification and
-- b) no import list
imported_modules :: TcGblEnv -> Map ModuleName [ModuleName]
imported_modules tcGblEnv
  | Just{} <- (export_list tcGblEnv) =
      unrestrictedModuleImports (fmap unLoc (tcg_rn_imports tcGblEnv))
  | otherwise =
      M.empty

decls ::
  (ReportErrorMessage m) => TcGblEnv -> m [(GenLocated SrcSpanAnnA (HsDecl GhcRn), [HsDoc GhcRn])]
decls tcGblEnv =
  case (tcg_rn_decls tcGblEnv) of
    Nothing -> do
      reportErrorMessage "Warning: Renamed source is not available"
      pure []
    Just dx ->
      pure (topDecls dx)

-- All elements of an explicit export list, if present
export_list :: TcGblEnv -> Maybe [(IE GhcRn, Avails)]
export_list tcGblEnv =
  case (tcg_rn_exports tcGblEnv) of
    Just rn_exports ->
      Just [(ie, avail) | (L _ ie, avail) <- rn_exports]
    Nothing -> Nothing

exported_names :: TcGblEnv -> [Name]
exported_names =
  concatMap availNamesWithSelectors . tcg_exports

-- We want to know which modules are imported without any qualification. This
-- way we can display module reexports more compactly. This mapping also looks
-- through aliases:
--
-- module M (module X) where
--   import M1 as X
--   import M2 as X
--
-- With our mapping we know that we can display exported modules M1 and M2.
--
unrestrictedModuleImports :: [ImportDecl GhcRn] -> M.Map ModuleName [ModuleName]
unrestrictedModuleImports idecls =
  M.map (map (unLoc . ideclName)) $
    M.filter (all isInteresting) impModMap
 where
  impModMap =
    M.fromListWith (++) (concatMap moduleMapping idecls)

  moduleMapping idecl =
    concat
      [ [(unLoc (ideclName idecl), [idecl])]
      , [ (unLoc mod_name, [idecl])
        | Just mod_name <- [ideclAs idecl]
        ]
      ]

  isInteresting idecl =
    case ideclHiding idecl of
      -- i) no subset selected
      Nothing -> True
      -- ii) an import with a hiding clause
      -- without any names
      Just (True, L _ []) -> True
      -- iii) any other case of qualification
      _ -> False

-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

mkWarningMap :: DynFlags -> Warnings a -> GlobalRdrEnv -> [Name] -> ErrMsgM WarningMap
mkWarningMap dflags warnings gre exps = case warnings of
  NoWarnings -> pure M.empty
  WarnAll _ -> pure M.empty
  WarnSome ws ->
    let ws' =
          [ (n, w)
          | (occ, w) <- ws
          , elt <- lookupGlobalRdrEnv gre occ
          , let n = greMangledName elt
          , n `elem` exps
          ]
     in M.fromList Applicative.<$> traverse (bitraverse pure (parseWarning dflags gre)) ws'

parseWarning :: DynFlags -> GlobalRdrEnv -> WarningTxt a -> ErrMsgM (Doc Name)
parseWarning dflags gre w = case w of
  DeprecatedTxt _ msg -> format "Deprecated: " (foldMap (unpackFS . sl_fs . hsDocString . unLoc) msg)
  WarningTxt _ msg -> format "Warning: " (foldMap (unpackFS . sl_fs . hsDocString . unLoc) msg)
 where
  format x bs =
    DocWarning . DocParagraph . DocAppend (DocString x)
      Applicative.<$> processDocStringFromString dflags gre bs

--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------

type Pollock_Maps =
  ( DocMap Name
  , ArgMap Name
  , Map Name [GenLocated SrcSpanAnnA (HsDecl GhcRn)]
  )

{- | Create 'Maps' by looping through the declarations. For each declaration,
find its names, its subordinates, and its doc strings. Process doc strings
into 'Doc's.
-}
pollock_mkMaps ::
  DynFlags
  -> Maybe Package -- this package
  -> GlobalRdrEnv
  -> [Name]
  -> [(LHsDecl GhcRn, [HsDoc GhcRn])]
  -> ExtractedTHDocs
  -- ^ Template Haskell putDoc docs
  -> ErrMsgM Pollock_Maps
pollock_mkMaps dflags pkgName gre instances hsdecls thDocs = do
  (a, b, c) <- unzip3 Applicative.<$> traverse mappings hsdecls
  (th_a, th_b) <- thMappings
  pure
    ( th_a `M.union` f' (map (nubByName fst) a)
    , fmap intmap2mapint $
        th_b `unionArgMaps` (f (filterMapping (not . IM.null) b))
    , f (filterMapping (not . null) c)
    )
 where
  f :: (Ord a, Monoid b) => [[(a, b)]] -> Map a b
  f = M.fromListWith (<>) . concat

  f' :: [[(Name, MDoc Name)]] -> Map Name (MDoc Name)
  f' = M.fromListWith metaDocAppend . concat

  filterMapping :: (b -> Bool) -> [[(a, b)]] -> [[(a, b)]]
  filterMapping p = map (filter (p . snd))

  -- Convert IntMap -> IntMap
  -- TODO: should ArgMap eventually be switched over to IntMap?
  intmap2mapint = M.fromList . IM.toList

  -- \| Extract the mappings from template haskell.
  -- No DeclMap/InstMap is needed since we already have access to the
  -- doc strings
  thMappings :: ErrMsgM (Map Name (MDoc Name), Map Name (IntMap (MDoc Name)))
  thMappings = do
    let ExtractedTHDocs
          _
          declDocs
          argDocs
          instDocs = thDocs
        ds2mdoc :: (HsDoc GhcRn) -> ErrMsgM (MDoc Name)
        ds2mdoc = processDocStringParas dflags pkgName gre . hsDocString

    let cvt = M.fromList . nonDetEltsUniqMap

    declDocs' <- mapM ds2mdoc (cvt declDocs)
    argDocs' <- mapM (mapM ds2mdoc) (cvt argDocs)
    instDocs' <- mapM ds2mdoc (cvt instDocs)
    return (declDocs' <> instDocs', argDocs')

  mappings ::
    (LHsDecl GhcRn, [HsDoc GhcRn])
    -> ErrMsgM
        ( [(Name, MDoc Name)]
        , [(Name, IntMap (MDoc Name))]
        , [(Name, [LHsDecl GhcRn])]
        )
  mappings (ldecl@(L (SrcSpanAnn _ (RealSrcSpan l _)) decl), hs_docStrs) = do
    let docStrs = map hsDocString hs_docStrs
        declDoc ::
          [HsDocString]
          -> IntMap HsDocString
          -> ErrMsgM (Maybe (MDoc Name), IntMap (MDoc Name))
        declDoc strs m = do
          doc' <- processDocStrings dflags pkgName gre strs
          m' <- traverse (processDocStringParas dflags pkgName gre) m
          pure (doc', m')

    (doc, args) <- declDoc docStrs (fmap hsDocString (declTypeDocs decl))

    let
      subs :: [(Name, [HsDocString], IntMap HsDocString)]
      subs =
        map (\(n, ds, im) -> (n, map hsDocString ds, fmap hsDocString im)) $
          subordinates emptyOccEnv instanceMap decl

    (subDocs, subArgs) <- unzip Applicative.<$> traverse (\(_, strs, m) -> declDoc strs m) subs

    let
      ns = names l decl
      subNs = [n | (n, _, _) <- subs]
      dm = [(n, d) | (n, Just d) <- zip ns (repeat doc) ++ zip subNs subDocs]
      am = [(n, args) | n <- ns] ++ zip subNs subArgs
      cm = [(n, [ldecl]) | n <- ns ++ subNs]

    seqList ns `seq`
      seqList subNs `seq`
        doc `seq`
          seqList subDocs `seq`
            seqList subArgs `seq`
              pure (dm, am, cm)
  mappings (L (SrcSpanAnn _ (UnhelpfulSpan _)) _, _) = pure ([], [], [])

  instanceMap :: Map RealSrcSpan Name
  instanceMap = M.fromList [(l, n) | n <- instances, RealSrcSpan l _ <- [getSrcSpan n]]

  names :: RealSrcSpan -> HsDecl GhcRn -> [Name]
  names _ (InstD _ d) = maybeToList (SrcLoc.lookupSrcSpan loc instanceMap) -- See note [2].
   where
    loc = case d of
      -- The CoAx's loc is the whole line, but only for TFs. The
      -- workaround is to dig into the family instance declaration and
      -- get the identifier with the right location.
      TyFamInstD _ (TyFamInstDecl _ d') -> getLocA (feqn_tycon d')
      _ -> getInstLoc d
  names l (DerivD{}) = maybeToList (M.lookup l instanceMap) -- See note [2].
  names _ decl = getMainDeclBinder emptyOccEnv decl

{- | Unions together two 'ArgDocMaps' (or ArgMaps in haddock-api), such that two
maps with values for the same key merge the inner map as well.
Left biased so @unionArgMaps a b@ prefers @a@ over @b@.
-}
unionArgMaps ::
  forall b.
  Map Name (IntMap b)
  -> Map Name (IntMap b)
  -> Map Name (IntMap b)
unionArgMaps a b = M.foldrWithKey go b a
 where
  go ::
    Name
    -> IntMap b
    -> Map Name (IntMap b)
    -> Map Name (IntMap b)
  go n newArgMap acc
    | Just oldArgMap <- M.lookup n acc =
        M.insert n (newArgMap `IM.union` oldArgMap) acc
    | otherwise = M.insert n newArgMap acc

-- Note [2]:
------------
-- We relate ClsInsts to InstDecls and DerivDecls using the SrcSpans buried
-- inside them. That should work for normal user-written instances (from
-- looking at GHC sources). We can assume that commented instances are
-- user-written. This lets us relate Names (from ClsInsts) to comments
-- (associated with InstDecls and DerivDecls).

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

{- | Build the list of items that will become the documentation, from the
export list.  At this point, the list of ExportItems is in terms of
original names.

We create the export items even if the module is hidden, since they
might be useful when creating the export items for other modules.
-}
pollock_mkExportItems ::
  (Monad m) =>
  Bool -- is it a signature
  -> Maybe Package -- this package
  -> Module -- this module
  -> Module -- semantic module
  -> WarningMap
  -> GlobalRdrEnv
  -> [Name] -- exported names (orig)
  -> [LHsDecl GhcRn] -- renamed source declarations
  -> Pollock_Maps
  -> M.Map ModuleName [ModuleName]
  -> Maybe [(IE GhcRn, Avails)]
  -> Avails -- exported stuff from this module
  -> DynFlags
  -> IfM m [Pollock_ExportItem GhcRn]
pollock_mkExportItems
  is_sig
  pkgName
  thisMod
  semMod
  warnings
  gre
  exportedNames
  hsdecls
  maps
  unrestricted_imp_mods
  exportList
  allExports
  dflags =
    case exportList of
      Nothing ->
        pollock_fullModuleContents
          is_sig
          pkgName
          thisMod
          semMod
          warnings
          gre
          exportedNames
          hsdecls
          maps
          dflags
          allExports
      Just exports -> liftM concat $ mapM lookupExport exports
   where
    lookupExport (IEGroup _ lev docStr, _) = liftErrMsg $ do
      doc <- processDocString dflags gre (hsDocString . unLoc $ docStr)
      return [pollock_mkExportGroup lev doc]
    lookupExport (IEDoc _ docStr, _) = liftErrMsg $ do
      doc <- processDocStringParas dflags pkgName gre (hsDocString . unLoc $ docStr)
      return [pollock_mkExportDoc doc]
    lookupExport (IEDocNamed _ str, _) =
      liftErrMsg $
        findNamedDoc str [unL d | d <- hsdecls] >>= \case
          Nothing -> return []
          Just docStr -> do
            doc <- processDocStringParas dflags pkgName gre docStr
            return [pollock_mkExportDoc doc]
    lookupExport (IEModuleContents _ (L _ mod_name), _)
      -- only consider exporting a module if we are sure we
      -- are really exporting the whole module and not some
      -- subset. We also look through module aliases here.
      | Just mods <- M.lookup mod_name unrestricted_imp_mods
      , not (null mods) =
          pure []
    -- POL-TODO Can we get away with completely ignoring module exports like this?
    -- concat <$> traverse (moduleExport thisMod dflags modMap instIfaceMap) mods

    lookupExport (_, avails) =
      concat Applicative.<$> traverse availExport (nubAvails avails)

    availExport avail =
      pollock_availExportItem
        is_sig
        thisMod
        semMod
        warnings
        exportedNames
        maps
        dflags
        avail

-- Extract the minimal complete definition of a Name, if one exists
minimalDef :: (Monad m) => Name -> IfM m (Maybe ClassMinimalDef)
minimalDef n = do
  mty <- lookupName n
  case mty of
    Just (ATyCon a) ->
      pure . fmap classMinimalDef $ tyConClass_maybe a
    _ ->
      return Nothing

pollock_availExportItem ::
  forall m.
  (Monad m) =>
  Bool -- is it a signature
  -> Module -- this module
  -> Module -- semantic module
  -> WarningMap
  -> [Name] -- exported names (orig)
  -> Pollock_Maps
  -> DynFlags
  -> AvailInfo
  -> IfM m [Pollock_ExportItem GhcRn]
pollock_availExportItem
  is_sig
  thisMod
  semMod
  warnings
  exportedNames
  (docMap, argMap, declMap)
  dflags
  availInfo = declWith availInfo
   where
    declWith :: AvailInfo -> IfM m [Pollock_ExportItem GhcRn]
    declWith avail = do
      let t = availName avail
      r <- findDecl avail
      case r of
        ([L l' (ValD _ _)], (doc, _)) -> do
          let l = locA l'
          -- Top-level binding without type signature
          export <- pollock_hiValExportItem dflags t l doc
          return [export]
        (ds, docs_)
          | decl : _ <- filter (not . isValD . unLoc) ds ->
              let declNames = getMainDeclBinder emptyOccEnv (unL decl)
               in case () of
                    _
                      -- We should not show a subordinate by itself if any of its
                      -- parents is also exported. See note [1].
                      | t `notElem` declNames
                      , Just p <- find isExported (parents t $ unL decl) ->
                          do
                            reportErrorMessage $
                              "Warning: "
                                <> errMsgFromString (moduleString thisMod)
                                <> ": "
                                <> errMsgFromString (pretty dflags (nameOccName t))
                                <> " is exported separately but "
                                <> "will be documented under "
                                <> errMsgFromString (pretty dflags (nameOccName p))
                                <> ". Consider exporting it together with its parent(s)"
                                <> " for code clarity."
                            return []

                      -- normal case
                      | otherwise -> case decl of
                          -- A single signature might refer to many names, but we
                          -- create an export item for a single name only.  So we
                          -- modify the signature to contain only that single name.
                          L loc (SigD _ sig) ->
                            -- fromJust is safe since we already checked in guards
                            -- that 't' is a name declared in this declaration.
                            let newDecl = L loc . SigD noExtField . fromJust $ filterSigNames (== t) sig
                             in availExportDecl avail newDecl docs_
                          L loc (TyClD _ ClassDecl{..}) -> do
                            mdef <- minimalDef t
                            let sig = maybeToList $ fmap (noLocA . MinimalSig noAnn NoSourceText . noLocA . fmap noLocA) mdef
                            availExportDecl
                              avail
                              (L loc $ TyClD noExtField ClassDecl{tcdSigs = sig ++ tcdSigs, ..})
                              docs_
                          _ -> availExportDecl avail decl docs_
        -- Declaration from another package
        ([], _) -> pure [] -- specifically do not care about other packages
        _ -> return []

    -- Tries 'extractDecl' first then falls back to 'hiDecl' if that fails
    availDecl :: Name -> LHsDecl GhcRn -> IfM m (LHsDecl GhcRn)
    availDecl declName parentDecl =
      case extractDecl declMap declName parentDecl of
        Right d -> pure d
        Left err -> do
          synifiedDeclOpt <- hiDecl dflags declName
          case synifiedDeclOpt of
            Just synifiedDecl -> pure synifiedDecl
            Nothing -> pprPanic "availExportItem" (O.text (show err))

    availExportDecl ::
      AvailInfo
      -> LHsDecl GhcRn
      -> (DocForDecl Name, [(Name, DocForDecl Name)])
      -> IfM m [Pollock_ExportItem GhcRn]
    availExportDecl avail decl (doc, subs)
      | availExportsDecl avail = do
          extractedDecl <- availDecl (availName avail) decl

          -- bundled pattern synonyms only make sense if the declaration is
          -- exported (otherwise there would be nothing to bundle to)
          bundledPatSyns <- findBundledPatterns avail

          return
            [ Pollock_ExportItemDecl $
                Pollock_ExportDecl
                  { pollock_expItemDecl = restrictTo (fmap fst subs) extractedDecl
                  , pollock_expItemPats = bundledPatSyns
                  , pollock_expItemMbDoc = doc
                  , pollock_expItemSubDocs = subs
                  }
            ]
      | otherwise = for subs $ \(sub, sub_doc) -> do
          extractedDecl <- availDecl sub decl

          return
            ( Pollock_ExportItemDecl $
                Pollock_ExportDecl
                  { pollock_expItemDecl = extractedDecl
                  , pollock_expItemPats = []
                  , pollock_expItemMbDoc = sub_doc
                  , pollock_expItemSubDocs = []
                  }
            )

    exportedNameSet = mkNameSet exportedNames
    isExported n = elemNameSet n exportedNameSet

    findDecl :: AvailInfo -> IfM m ([LHsDecl GhcRn], (DocForDecl Name, [(Name, DocForDecl Name)]))
    findDecl avail
      | m == semMod =
          case M.lookup n declMap of
            Just ds -> return (ds, lookupDocs avail warnings docMap argMap)
            Nothing
              | is_sig -> do
                  -- OK, so it wasn't in the local declaration map.  It could
                  -- have been inherited from a signature.  Reconstitute it
                  -- from the type.
                  mb_r <- hiDecl dflags n
                  case mb_r of
                    Nothing -> return ([], (noDocForDecl, availNoDocs avail))
                    -- TODO: If we try harder, we might be able to find
                    -- a Haddock!  Look in the Haddocks for each thing in
                    -- requirementContext (unitState)
                    Just decl -> return ([decl], (noDocForDecl, availNoDocs avail))
              | otherwise ->
                  return ([], (noDocForDecl, availNoDocs avail))
      -- POL-TODO I _think_ our purposes can simply ignore this, we'll be dealing with a source
      -- module pretty much always anyway.
      -- \| Just iface <- M.lookup (semToIdMod (moduleUnit thisMod) m) modMap
      -- , Just ds <- M.lookup n (ifaceDeclMap iface) =
      --     return (ds, lookupDocs avail warnings
      --                       (ifaceDocMap iface)
      --                       (ifaceArgMap iface))
      | otherwise = return ([], (noDocForDecl, availNoDocs avail))
     where
      n = availName avail
      m = nameModule n

    findBundledPatterns :: AvailInfo -> IfM m [(HsDecl GhcRn, DocForDecl Name)]
    findBundledPatterns avail = do
      patsyns <- for constructor_names $ \name -> do
        mtyThing <- lookupName name
        case mtyThing of
          Just (AConLike PatSynCon{}) -> do
            export_items <- declWith (Avail.avail name)
            pure
              [ (unLoc patsyn_decl, patsyn_doc)
              | Pollock_ExportItemDecl
                  ( Pollock_ExportDecl
                      { pollock_expItemDecl = patsyn_decl
                      , pollock_expItemMbDoc = patsyn_doc
                      }
                    ) <-
                  export_items
              ]
          _ -> pure []
      pure (concat patsyns)
     where
      constructor_names =
        filter isDataConName (availSubordinates avail)

availSubordinates :: AvailInfo -> [Name]
availSubordinates = map greNameMangledName . availSubordinateGreNames

availNoDocs :: AvailInfo -> [(Name, DocForDecl Name)]
availNoDocs avail =
  zip (availSubordinates avail) (repeat noDocForDecl)

hiDecl :: (Monad m) => DynFlags -> Name -> IfM m (Maybe (LHsDecl GhcRn))
hiDecl dflags t = do
  mayTyThing <- lookupName t
  case mayTyThing of
    Nothing -> do
      reportErrorMessage ("Warning: Not found in environment: " <> errMsgFromString (pretty dflags t))
      return Nothing
    Just x -> case tyThingToLHsDecl ShowRuntimeRep x of
      Left m -> do
        reportErrorMessage (bugWarn m)
        return Nothing
      Right (m, t') -> do
        traverse_ (reportErrorMessage . bugWarn) m
        return (Just $ noLocA t')
 where
  warnLine x =
    O.text "haddock-bug:"
      O.<+> O.text (errMsgToString x)
      O.<> O.comma
      O.<+> O.quotes (O.ppr t)
      O.<+> O.text "-- Please report this on Haddock issue tracker!"
  bugWarn = errMsgFromString . showSDoc dflags . warnLine

{- | A version of 'hiValExportItem' with a bunch of stuff removed in attempt to cut down for the
coverage use case.
-}
pollock_hiValExportItem ::
  (Monad m) =>
  DynFlags
  -> Name
  -> SrcSpan
  -> DocForDecl Name
  -> IfM m (Pollock_ExportItem GhcRn)
pollock_hiValExportItem dflags name nLoc doc = do
  mayDecl <- hiDecl dflags name
  case mayDecl of
    Nothing -> return $ Pollock_ExportItemNoDecl (Pollock_ExportNoDecl name [])
    Just decl -> return $ Pollock_ExportItemDecl (Pollock_ExportDecl (fixSpan decl) [] doc [])
 where
  fixSpan (L (SrcSpanAnn a l) t) = L (SrcSpanAnn a (SrcLoc.combineSrcSpans l nLoc)) t

-- | Lookup docs for a declaration from maps.
lookupDocs ::
  AvailInfo
  -> WarningMap
  -> DocMap Name
  -> ArgMap Name
  -> (DocForDecl Name, [(Name, DocForDecl Name)])
lookupDocs avail warnings docMap argMap =
  let n = availName avail
   in let lookupArgDoc x = M.findWithDefault M.empty x argMap
       in let doc = (lookupDoc n, lookupArgDoc n)
           in let subDocs =
                    [ (s, (lookupDoc s, lookupArgDoc s))
                    | s <- availSubordinates avail
                    ]
               in (doc, subDocs)
 where
  lookupDoc name = Documentation (M.lookup name docMap) (M.lookup name warnings)

pollock_fullModuleContents ::
  (Monad m) =>
  Bool -- is it a signature
  -> Maybe Package -- this package
  -> Module -- this module
  -> Module -- semantic module
  -> WarningMap
  -> GlobalRdrEnv
  -- ^ The renaming environment
  -> [Name] -- exported names (orig)
  -> [LHsDecl GhcRn] -- renamed source declarations
  -> Pollock_Maps
  -> DynFlags
  -> Avails
  -> IfM m [Pollock_ExportItem GhcRn]
pollock_fullModuleContents
  is_sig
  pkgName
  thisMod
  semMod
  warnings
  gre
  exportedNames
  hsdecls
  maps@(_, _, declMap)
  dflags
  avails = do
    let availEnv = availsToNameEnv (nubAvails avails)
    (concat . concat)
      `fmap` ( for hsdecls $ \decl -> do
                case decl of
                  (L _ (DocD _ (DocGroup lev docStr))) -> do
                    doc <- liftErrMsg (processDocString dflags gre (hsDocString . unLoc $ docStr))
                    return [[pollock_mkExportGroup lev doc]]
                  (L _ (DocD _ (DocCommentNamed _ docStr))) -> do
                    doc <- liftErrMsg (processDocStringParas dflags pkgName gre (hsDocString . unLoc $ docStr))
                    return [[pollock_mkExportDoc doc]]
                  (L _ (ValD _ valDecl))
                    | name : _ <- collectHsBindBinders CollNoDictBinders valDecl
                    , Just (L _ SigD{} : _) <- filter isSigD Applicative.<$> M.lookup name declMap ->
                        return []
                  _ ->
                    for (getMainDeclBinder emptyOccEnv (unLoc decl)) $ \nm -> do
                      case lookupNameEnv availEnv nm of
                        Just avail ->
                          pollock_availExportItem
                            is_sig
                            thisMod
                            semMod
                            warnings
                            exportedNames
                            maps
                            dflags
                            avail
                        Nothing -> pure []
             )
   where
    isSigD (L _ SigD{}) = True
    isSigD _ = False

{- | Sometimes the declaration we want to export is not the "main" declaration:
it might be an individual record selector or a class method.  In these
cases we have to extract the required declaration (and somehow cobble
together a type signature for it...).

This function looks through the declarations in this module to try to find
the one with the right name.
-}
extractDecl ::
  (HasCallStack) =>
  DeclMap
  -- ^ all declarations in the file
  -> Name
  -- ^ name of the declaration to extract
  -> LHsDecl GhcRn
  -- ^ parent declaration
  -> Either ErrMsg (LHsDecl GhcRn)
extractDecl declMap name decl
  | name `elem` getMainDeclBinder emptyOccEnv (unLoc decl) = pure decl
  | otherwise =
      case unLoc decl of
        TyClD
          _
          d@ClassDecl
            { tcdLName = L _ clsNm
            , tcdSigs = clsSigs
            , tcdATs = clsATs
            } ->
            let
              matchesMethod =
                [ lsig
                | lsig <- clsSigs
                , ClassOpSig _ False _ _ <- pure $ unLoc lsig
                , -- Note: exclude `default` declarations (see #505)
                name `elem` sigName lsig
                ]

              matchesAssociatedType =
                [ lfam_decl
                | lfam_decl <- clsATs
                , name == unLoc (fdLName (unLoc lfam_decl))
                ]
             in
              -- TODO: document fixity
              case (matchesMethod, matchesAssociatedType) of
                ([s0], _) ->
                  let tyvar_names = tyClDeclTyVars d
                      L pos sig = addClassContext clsNm tyvar_names s0
                   in pure (L pos (SigD noExtField sig))
                (_, [L pos fam_decl]) -> pure (L pos (TyClD noExtField (FamDecl noExtField fam_decl)))
                ([], [])
                  | Just (famInstDecl : _) <- M.lookup name declMap ->
                      extractDecl declMap name famInstDecl
                _ ->
                  Left
                    ( mconcat
                        [ "Ambiguous decl for "
                        , errMsgFromString (getOccString name)
                        , " in class "
                        , errMsgFromString (getOccString clsNm)
                        ]
                    )
        TyClD
          _
          d@DataDecl
            { tcdLName = L _ dataNm
            , tcdDataDefn = HsDataDefn{dd_cons = dataCons}
            } -> do
            let ty_args = lHsQTyVarsToTypes (tyClDeclTyVars d)
            lsig <-
              if isDataConName name
                then extractPatternSyn name dataNm ty_args dataCons
                else extractRecSel name dataNm ty_args dataCons
            pure (SigD noExtField Applicative.<$> lsig)
        TyClD _ FamDecl{}
          | isValName name
          , Just (famInst : _) <- M.lookup name declMap ->
              extractDecl declMap name famInst
        InstD
          _
          ( DataFamInstD
              _
              ( DataFamInstDecl
                  ( FamEqn
                      { feqn_tycon = L _ n
                      , feqn_pats = tys
                      , feqn_rhs = defn
                      }
                    )
                )
            ) ->
            if isDataConName name
              then fmap (SigD noExtField) Applicative.<$> extractPatternSyn name n tys (dd_cons defn)
              else fmap (SigD noExtField) Applicative.<$> extractRecSel name n tys (dd_cons defn)
        InstD _ (ClsInstD _ ClsInstDecl{cid_datafam_insts = insts})
          | isDataConName name ->
              let matches =
                    [ d' | L _ d'@(DataFamInstDecl (FamEqn{feqn_rhs = dd})) <- insts, name `elem` map unLoc (concatMap (getConNames . unLoc) (dd_cons dd))
                    ]
               in case matches of
                    [d0] -> extractDecl declMap name (noLocA (InstD noExtField (DataFamInstD noExtField d0)))
                    _ -> Left "internal: extractDecl (ClsInstD)"
          | otherwise ->
              let matches =
                    [ d'
                    | L _ d'@(DataFamInstDecl d) <-
                        insts
                    , -- , L _ ConDecl { con_details = RecCon rec } <- dd_cons (feqn_rhs d)
                    Just rec <- map (getRecConArgs_maybe . unLoc) (dd_cons (feqn_rhs d))
                    , ConDeclField{cd_fld_names = ns} <- map unLoc (unLoc rec)
                    , L _ n <- ns
                    , foExt n == name
                    ]
               in case matches of
                    [d0] -> extractDecl declMap name (noLocA . InstD noExtField $ DataFamInstD noExtField d0)
                    _ -> Left "internal: extractDecl (ClsInstD)"
        _ -> Left ("extractDecl: Unhandled decl for " <> String.fromString (getOccString name))

extractPatternSyn ::
  Name
  -> Name
  -> [LHsTypeArg GhcRn]
  -> [LConDecl GhcRn]
  -> Either ErrMsg (LSig GhcRn)
extractPatternSyn nm t tvs cons =
  case filter matches cons of
    [] ->
      Left . errMsgFromString . O.showSDocOneLine O.defaultSDocContext $
        O.text "constructor pattern " O.<+> O.ppr nm O.<+> O.text "not found in type" O.<+> O.ppr t
    con : _ -> pure (extract Applicative.<$> con)
 where
  matches :: LConDecl GhcRn -> Bool
  matches (L _ con) = nm `elem` (unLoc Applicative.<$> getConNames con)
  extract :: ConDecl GhcRn -> Sig GhcRn
  extract con =
    let args =
          case con of
            ConDeclH98{con_args = con_args'} -> case con_args' of
              PrefixCon _ args' -> map hsScaledThing args'
              RecCon (L _ fields) -> cd_fld_type . unLoc Applicative.<$> fields
              InfixCon arg1 arg2 -> map hsScaledThing [arg1, arg2]
            ConDeclGADT{con_g_args = con_args'} -> case con_args' of
              PrefixConGADT args' -> map hsScaledThing args'
              RecConGADT (L _ fields) _ -> cd_fld_type . unLoc Applicative.<$> fields
        typ = longArrow args (data_ty con)
        typ' =
          case con of
            ConDeclH98{con_mb_cxt = Just cxt} -> noLocA (HsQualTy noExtField cxt typ)
            _ -> typ
        typ'' = noLocA (HsQualTy noExtField (noLocA []) typ')
     in PatSynSig noAnn [noLocA nm] (mkEmptySigType typ'')

  longArrow :: [LHsType GhcRn] -> LHsType GhcRn -> LHsType GhcRn
  longArrow inputs output = foldr (\x y -> noLocA (HsFunTy noAnn (HsUnrestrictedArrow noHsUniTok) x y)) output inputs

  data_ty con
    | ConDeclGADT{} <- con = con_res_ty con
    | otherwise =
        foldl' (\x y -> noLocA (mkAppTyArg x y)) (noLocA (HsTyVar noAnn NotPromoted (noLocA t))) tvs
   where
    mkAppTyArg :: LHsType GhcRn -> LHsTypeArg GhcRn -> HsType GhcRn
    mkAppTyArg f (HsValArg ty) = HsAppTy noExtField f ty
    mkAppTyArg f (HsTypeArg l ki) = HsAppKindTy l f ki
    mkAppTyArg f (HsArgPar _) = HsParTy noAnn f

extractRecSel ::
  Name
  -> Name
  -> [LHsTypeArg GhcRn]
  -> [LConDecl GhcRn]
  -> Either ErrMsg (LSig GhcRn)
extractRecSel _ _ _ [] = Left "extractRecSel: selector not found"
extractRecSel nm t tvs (L _ con : rest) =
  case getRecConArgs_maybe con of
    Just (L _ fields)
      | ((l, L _ (ConDeclField _ _nn ty _)) : _) <- matching_fields fields ->
          pure
            ( L
                (noAnnSrcSpan l)
                ( TypeSig
                    noAnn
                    [noLocA nm]
                    ( mkEmptyWildCardBndrs $
                        mkEmptySigType (noLocA (HsFunTy noAnn (HsUnrestrictedArrow noHsUniTok) data_ty (getBangType ty)))
                    )
                )
            )
    _ -> extractRecSel nm t tvs rest
 where
  matching_fields :: [LConDeclField GhcRn] -> [(SrcSpan, LConDeclField GhcRn)]
  matching_fields flds =
    [ (locA l, f) | f@(L _ (ConDeclField _ ns _ _)) <- flds, L l n <- ns, foExt n == nm
    ]
  data_ty
    -- ResTyGADT _ ty <- con_res con = ty
    | ConDeclGADT{} <- con = con_res_ty con
    | otherwise =
        foldl' (\x y -> noLocA (mkAppTyArg x y)) (noLocA (HsTyVar noAnn NotPromoted (noLocA t))) tvs
   where
    mkAppTyArg :: LHsType GhcRn -> LHsTypeArg GhcRn -> HsType GhcRn
    mkAppTyArg f (HsValArg ty) = HsAppTy noExtField f ty
    mkAppTyArg f (HsTypeArg l ki) = HsAppKindTy l f ki
    mkAppTyArg f (HsArgPar _) = HsParTy noAnn f

-- | Keep export items with docs.
pruneExportItems :: [Pollock_ExportItem GhcRn] -> [Pollock_ExportItem GhcRn]
pruneExportItems = filter hasDoc
 where
  hasDoc (Pollock_ExportItemDecl (Pollock_ExportDecl{pollock_expItemMbDoc = (Documentation d _, _)})) = isJust d
  hasDoc _ = True

seqList :: [a] -> ()
seqList [] = ()
seqList (x : xs) = x `seq` seqList xs

-- | Find a stand-alone documentation comment by its name.
findNamedDoc :: String -> [HsDecl GhcRn] -> ErrMsgM (Maybe HsDocString)
findNamedDoc name = search
 where
  search [] = do
    reportErrorMessage ("Cannot find documentation for: $" <> errMsgFromString name)
    return Nothing
  search (DocD _ (DocCommentNamed name' doc) : rest)
    | name == name' = return (Just (hsDocString . unLoc $ doc))
    | otherwise = search rest
  search (_other_decl : rest) = search rest
