{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
Module      :  Pollock
Copyright   :  (c) Trevis Elsesr 2023
License     :  MIT

Maintainer  :  trevis@flipstone.com
Stability   :  experimental
Portability :  portable
-}
module Pollock (IfM, runIfM, pollock_getCoverage) where

import Haddock.Convert (PrintRuntimeReps (..), tyThingToLHsDecl)
import Haddock.Doc (metaDocAppend)
import Haddock.GhcUtils
  ( addClassContext
  , filterSigNames
  , lHsQTyVarsToTypes
  , mkEmptySigType
  , pretty
  , restrictTo
  , sigName
  )
import Haddock.Interface.LexParseRn
  ( processDocStringParas
  , processDocStrings
  , processModuleHeader
  )
import qualified Haddock.Parser.Monad as PM
import Haddock.Types

import qualified Control.Applicative as Applicative
import qualified Control.Monad.IO.Class as MIO
import Control.Monad.Reader as Reader (MonadReader (..), ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Control.Monad.Writer (MonadWriter (..))
import Data.Foldable (traverse_)
import qualified Data.IntMap as IM
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as T
import Data.Traversable (for)

import qualified Pollock.CompatGHC as CompatGHC

newtype IfEnv m = IfEnv
  { ife_lookup_name :: CompatGHC.Name -> m (Maybe CompatGHC.TyThing)
  -- ^ Lookup names in the environment.
  }

{- | A monad in which we create Haddock interfaces. Not to be confused with
`GHC.Tc.Types.IfM` which is used to write GHC interfaces.

In the past `createInterface` was running in the `Ghc` monad but proved hard
to sustain as soon as we moved over for Haddock to be a plugin. Also abstracting
over the Ghc specific clarifies where side effects happen.
-}
newtype IfM m a = IfM {unIfM :: Reader.ReaderT (IfEnv m) (WriterT ErrorMessages m) a}

deriving newtype instance (Functor m) => Functor (IfM m)
deriving newtype instance (Monad m) => Applicative (IfM m)
deriving newtype instance (Monad m) => Monad (IfM m)
deriving newtype instance (MIO.MonadIO m) => MIO.MonadIO (IfM m)
deriving newtype instance (Monad m) => Reader.MonadReader (IfEnv m) (IfM m)
deriving newtype instance (Monad m) => MonadWriter ErrorMessages (IfM m)
deriving newtype instance (Monad m) => ReportErrorMessage (IfM m)

-- | Run an `IfM` action.
runIfM ::
  -- \| Lookup a global name in the current session. Used in cases
  -- where declarations don't
  (Functor m) =>
  (CompatGHC.Name -> m (Maybe CompatGHC.TyThing))
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
  fmap (fmap errorMessagesToList) $ runWriterT (runReaderT (unIfM action) if_env)

liftErrMsg :: (Monad m) => ErrMsgM a -> IfM m a
liftErrMsg action = do
  IfM (writer (runErrMsgM action))

lookupName :: (Monad m) => CompatGHC.Name -> IfM m (Maybe CompatGHC.TyThing)
lookupName name = IfM $ do
  lookup_name <- Reader.asks ife_lookup_name
  lift $ lift (lookup_name name)

pollock_getCoverage ::
  (MIO.MonadIO m) =>
  CompatGHC.TcGblEnv
  -> CompatGHC.ModSummary
  -> IfM m (Int, Int, HaddockModInfo)
pollock_getCoverage tcGblEnv modSummary = do
  let
    dflags = CompatGHC.ms_hspp_opts modSummary

    -- TyThings that have instances defined in this module
    local_instances :: [CompatGHC.Name]
    local_instances =
      [ name
      | name <-
          fmap CompatGHC.getName (CompatGHC.tcg_insts tcGblEnv) <> fmap CompatGHC.getName (CompatGHC.tcg_fam_insts tcGblEnv)
      , CompatGHC.nameIsLocalOrFrom (CompatGHC.tcg_semantic_mod tcGblEnv) name
      ]

    exportedNames = exported_names tcGblEnv
    -- Warnings on declarations in this module
    decl_warnings = mkWarningMap (CompatGHC.tcg_warns tcGblEnv) (CompatGHC.tcg_rdr_env tcGblEnv) exportedNames

  ds <- decls tcGblEnv
  -- Infer module safety
  safety <- MIO.liftIO (CompatGHC.finalSafeMode dflags tcGblEnv)

  -- The docs added via Template Haskell's putDoc
  thDocs@CompatGHC.ExtractedTHDocs{CompatGHC.ethd_mod_header = thMbDocStr} <-
    MIO.liftIO . fmap CompatGHC.extractTHDocs . CompatGHC.readIORef $ CompatGHC.tcg_th_docs tcGblEnv

  -- Process the top-level module header documentation.
  let !info =
        processModuleHeader
          dflags
          safety
          ( fmap CompatGHC.hsDocString thMbDocStr
              Applicative.<|> (CompatGHC.hsDocString . CompatGHC.unLoc <$> (CompatGHC.tcg_doc_hdr tcGblEnv))
          )

  maps <- liftErrMsg (pollock_mkMaps local_instances ds thDocs)

  export_items <-
    pollock_mkExportItems
      (CompatGHC.tcg_semantic_mod tcGblEnv)
      decl_warnings
      (fmap fst ds)
      maps
      (imported_modules tcGblEnv)
      (export_list tcGblEnv)
      (CompatGHC.tcg_exports tcGblEnv)
      dflags
  let
    pruned_export_items = pruneExportItems export_items
    !haddockable = 1 + length export_items -- module + exports
    !haddocked = (if Maybe.isJust (CompatGHC.tcg_doc_hdr tcGblEnv) then 1 else 0) + length pruned_export_items

  pure (haddockable, haddocked, info)

-- Module imports of the form `import X`. Note that there is
-- a) no qualification and
-- b) no import list
imported_modules :: CompatGHC.TcGblEnv -> Map.Map CompatGHC.ModuleName [CompatGHC.ModuleName]
imported_modules tcGblEnv
  | Just{} <- (export_list tcGblEnv) =
      unrestrictedModuleImports (fmap CompatGHC.unLoc (CompatGHC.tcg_rn_imports tcGblEnv))
  | otherwise =
      Map.empty

decls ::
  (ReportErrorMessage m) =>
  CompatGHC.TcGblEnv
  -> m [(CompatGHC.GenLocated CompatGHC.SrcSpanAnnA (CompatGHC.HsDecl CompatGHC.GhcRn), [CompatGHC.HsDoc CompatGHC.GhcRn])]
decls tcGblEnv =
  case (CompatGHC.tcg_rn_decls tcGblEnv) of
    Nothing -> do
      reportErrorMessage "Warning: Renamed source is not available"
      pure []
    Just dx ->
      pure (CompatGHC.topDecls dx)

-- All elements of an explicit export list, if present
export_list :: CompatGHC.TcGblEnv -> Maybe [(CompatGHC.IE CompatGHC.GhcRn, CompatGHC.Avails)]
export_list tcGblEnv =
  case (CompatGHC.tcg_rn_exports tcGblEnv) of
    Just rn_exports ->
      Just [(ie, avail') | (CompatGHC.L _ ie, avail') <- rn_exports]
    Nothing -> Nothing

exported_names :: CompatGHC.TcGblEnv -> [CompatGHC.Name]
exported_names =
  concatMap CompatGHC.availNamesWithSelectors . CompatGHC.tcg_exports

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
unrestrictedModuleImports :: [CompatGHC.ImportDecl CompatGHC.GhcRn] -> Map.Map CompatGHC.ModuleName [CompatGHC.ModuleName]
unrestrictedModuleImports idecls =
  Map.map (fmap (CompatGHC.unLoc . CompatGHC.ideclName)) $
    Map.filter (all isInteresting) impModMap
 where
  impModMap =
    Map.fromListWith (++) (concatMap moduleMapping idecls)

  moduleMapping idecl =
    concat
      [ [(CompatGHC.unLoc (CompatGHC.ideclName idecl), [idecl])]
      , [ (CompatGHC.unLoc mod_name, [idecl])
        | Just mod_name <- [CompatGHC.ideclAs idecl]
        ]
      ]

  isInteresting idecl =
    case CompatGHC.ideclHiding idecl of
      -- i) no subset selected
      Nothing -> True
      -- ii) an import with a hiding clause
      -- without any names
      Just (True, CompatGHC.L _ []) -> True
      -- iii) any other case of qualification
      _ -> False

-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

mkWarningMap ::
  CompatGHC.Warnings a -> CompatGHC.GlobalRdrEnv -> [CompatGHC.Name] -> WarningMap
mkWarningMap warnings gre exps = case warnings of
  CompatGHC.NoWarnings -> Map.empty
  CompatGHC.WarnAll _ -> Map.empty
  CompatGHC.WarnSome ws ->
    let ws' =
          [ (n, w)
          | (occ, w) <- ws
          , elt <- CompatGHC.lookupGlobalRdrEnv gre occ
          , let n = CompatGHC.greMangledName elt
          , n `elem` exps
          ]
     in Map.fromList $ (fmap . fmap) parseWarning ws'

parseWarning :: CompatGHC.WarningTxt a -> Doc
parseWarning w = case w of
  CompatGHC.DeprecatedTxt _ msg -> format "Deprecated: " (foldMap (CompatGHC.unpackFS . CompatGHC.sl_fs . CompatGHC.hsDocString . CompatGHC.unLoc) msg)
  CompatGHC.WarningTxt _ msg -> format "Warning: " (foldMap (CompatGHC.unpackFS . CompatGHC.sl_fs . CompatGHC.hsDocString . CompatGHC.unLoc) msg)
 where
  format :: String -> String -> Doc
  format x =
    DocWarning . DocParagraph . DocAppend (DocString x) . PM.parseText . T.pack

--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------

type Pollock_Maps =
  ( DocMap
  , ArgMap
  , Map.Map CompatGHC.Name [CompatGHC.GenLocated CompatGHC.SrcSpanAnnA (CompatGHC.HsDecl CompatGHC.GhcRn)]
  )

{- | Create 'Maps' by looping through the declarations. For each declaration,
find its names, its subordinates, and its doc strings. Process doc strings
into 'Doc's.
-}
pollock_mkMaps ::
  [CompatGHC.Name]
  -> [(CompatGHC.LHsDecl CompatGHC.GhcRn, [CompatGHC.HsDoc CompatGHC.GhcRn])]
  -> CompatGHC.ExtractedTHDocs
  -- ^ Template Haskell putDoc docs
  -> ErrMsgM Pollock_Maps
pollock_mkMaps instances hsdecls thDocs = do
  (a, b, c) <- unzip3 <$> traverse mappings hsdecls
  (th_a, th_b) <- thMappings
  pure
    ( th_a `Map.union` f' (fmap (CompatGHC.nubByName fst) a)
    , fmap intmap2mapint $
        th_b `unionArgMaps` (f (filterMapping (not . IM.null) b))
    , f (filterMapping (not . null) c)
    )
 where
  f :: (Ord a, Monoid b) => [[(a, b)]] -> Map.Map a b
  f = Map.fromListWith (<>) . concat

  f' :: [[(CompatGHC.Name, MDoc)]] -> Map.Map CompatGHC.Name MDoc
  f' = Map.fromListWith metaDocAppend . concat

  filterMapping :: (b -> Bool) -> [[(a, b)]] -> [[(a, b)]]
  filterMapping p = fmap (filter (p . snd))

  -- Convert IntMap -> IntMap
  -- TODO: should ArgMap eventually be switched over to IntMap?
  intmap2mapint = Map.fromList . IM.toList

  -- \| Extract the mappings from template haskell.
  -- No DeclMap/InstMap is needed since we already have access to the
  -- doc strings
  thMappings :: ErrMsgM (Map.Map CompatGHC.Name MDoc, Map.Map CompatGHC.Name (IM.IntMap MDoc))
  thMappings = do
    let CompatGHC.ExtractedTHDocs
          _
          declDocs
          argDocs
          instDocs = thDocs
        ds2mdoc :: (CompatGHC.HsDoc CompatGHC.GhcRn) -> MDoc
        ds2mdoc = processDocStringParas . CompatGHC.hsDocString

    let cvt = Map.fromList . CompatGHC.nonDetEltsUniqMap

        declDocs' = fmap ds2mdoc (cvt declDocs)
        argDocs' = fmap (fmap ds2mdoc) (cvt argDocs)
        instDocs' = fmap ds2mdoc (cvt instDocs)
    pure (declDocs' <> instDocs', argDocs')

  mappings ::
    (CompatGHC.LHsDecl CompatGHC.GhcRn, [CompatGHC.HsDoc CompatGHC.GhcRn])
    -> ErrMsgM
        ( [(CompatGHC.Name, MDoc)]
        , [(CompatGHC.Name, IM.IntMap MDoc)]
        , [(CompatGHC.Name, [CompatGHC.LHsDecl CompatGHC.GhcRn])]
        )
  mappings (ldecl@(CompatGHC.L (CompatGHC.SrcSpanAnn _ (CompatGHC.RealSrcSpan l _)) decl), hs_docStrs) = do
    let docStrs = fmap CompatGHC.hsDocString hs_docStrs
        declDoc ::
          [CompatGHC.HsDocString]
          -> IM.IntMap CompatGHC.HsDocString
          -> (Maybe MDoc, IM.IntMap MDoc)
        declDoc strs m =
          (processDocStrings strs, fmap processDocStringParas m)

        (doc', args) = declDoc docStrs (fmap CompatGHC.hsDocString (CompatGHC.declTypeDocs decl))

    let
      subs :: [(CompatGHC.Name, [CompatGHC.HsDocString], IM.IntMap CompatGHC.HsDocString)]
      subs =
        fmap (\(n, ds, im) -> (n, fmap CompatGHC.hsDocString ds, fmap CompatGHC.hsDocString im)) $
          CompatGHC.subordinates CompatGHC.emptyOccEnv instanceMap decl

      fn (_, strs, m) = declDoc strs m
      (subDocs, subArgs) = unzip $ fmap fn subs

    let
      ns = names l decl
      fst3 (a, _, _) = a
      subNs = fmap fst3 subs
      dm = [(n, d) | (n, Just d) <- zip ns (repeat doc') <> zip subNs subDocs]
      am = [(n, args) | n <- ns] <> zip subNs subArgs
      cm = [(n, [ldecl]) | n <- ns <> subNs]

    seqList ns `seq`
      seqList subNs `seq`
        doc' `seq`
          seqList subDocs `seq`
            seqList subArgs `seq`
              pure (dm, am, cm)
  mappings (CompatGHC.L (CompatGHC.SrcSpanAnn _ (CompatGHC.UnhelpfulSpan _)) _, _) = pure ([], [], [])

  instanceMap :: Map.Map CompatGHC.RealSrcSpan CompatGHC.Name
  instanceMap = Map.fromList [(l, n) | n <- instances, CompatGHC.RealSrcSpan l _ <- [CompatGHC.getSrcSpan n]]

  names :: CompatGHC.RealSrcSpan -> CompatGHC.HsDecl CompatGHC.GhcRn -> [CompatGHC.Name]
  names _ (CompatGHC.InstD _ d) = Maybe.maybeToList (CompatGHC.lookupSrcSpan loc instanceMap) -- See note [2].
   where
    loc = case d of
      -- The CoAx's loc is the whole line, but only for TFs. The
      -- workaround is to dig into the family instance declaration and
      -- get the identifier with the right location.
      CompatGHC.TyFamInstD _ (CompatGHC.TyFamInstDecl _ d') -> CompatGHC.getLocA (CompatGHC.feqn_tycon d')
      _ -> CompatGHC.getInstLoc d
  names l (CompatGHC.DerivD{}) = Maybe.maybeToList (Map.lookup l instanceMap) -- See note [2].
  names _ decl = CompatGHC.getMainDeclBinder CompatGHC.emptyOccEnv decl

{- | Unions together two 'ArgDocMaps' (or ArgMaps in haddock-api), such that two
maps with values for the same key merge the inner map as well.
Left biased so @unionArgMaps a b@ prefers @a@ over @b@.
-}
unionArgMaps ::
  forall b.
  Map.Map CompatGHC.Name (IM.IntMap b)
  -> Map.Map CompatGHC.Name (IM.IntMap b)
  -> Map.Map CompatGHC.Name (IM.IntMap b)
unionArgMaps a b = Map.foldrWithKey go b a
 where
  go ::
    CompatGHC.Name
    -> IM.IntMap b
    -> Map.Map CompatGHC.Name (IM.IntMap b)
    -> Map.Map CompatGHC.Name (IM.IntMap b)
  go n newArgMap acc
    | Just oldArgMap <- Map.lookup n acc =
        Map.insert n (newArgMap `IM.union` oldArgMap) acc
    | otherwise = Map.insert n newArgMap acc

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
  CompatGHC.Module -- semantic module
  -> WarningMap
  -> [CompatGHC.LHsDecl CompatGHC.GhcRn] -- renamed source declarations
  -> Pollock_Maps
  -> Map.Map CompatGHC.ModuleName [CompatGHC.ModuleName]
  -> Maybe [(CompatGHC.IE CompatGHC.GhcRn, CompatGHC.Avails)]
  -> CompatGHC.Avails -- exported stuff from this module
  -> CompatGHC.DynFlags
  -> IfM m [Pollock_ExportItem CompatGHC.GhcRn]
pollock_mkExportItems
  semMod
  warnings
  hsdecls
  maps
  unrestricted_imp_mods
  exportList
  allExports
  dflags =
    case exportList of
      Nothing ->
        pollock_fullModuleContents
          semMod
          warnings
          hsdecls
          maps
          dflags
          allExports
      Just exports -> fmap concat $ mapM lookupExport exports
   where
    lookupExport (CompatGHC.IEGroup _ _ _, _) =
      pure [pollock_mkExportGroup]
    lookupExport (CompatGHC.IEDoc _ docStr, _) =
      pure [pollock_mkExportDoc . processDocStringParas . CompatGHC.hsDocString $ CompatGHC.unLoc docStr]
    lookupExport (CompatGHC.IEDocNamed _ str, _) =
      liftErrMsg $
        findNamedDoc str (fmap CompatGHC.unLoc hsdecls) >>= \case
          Nothing -> pure []
          Just docStr ->
            pure [pollock_mkExportDoc $ processDocStringParas docStr]
    lookupExport (CompatGHC.IEModuleContents _ (CompatGHC.L _ mod_name), _)
      -- only consider exporting a module if we are sure we
      -- are really exporting the whole module and not some
      -- subset. We also look through module aliases here.
      | Just mods <- Map.lookup mod_name unrestricted_imp_mods
      , not (null mods) =
          pure []
    -- POL-TODO Can we get away with completely ignoring module exports like this?
    -- concat <$> traverse (moduleExport thisMod dflags modMap instIfaceMap) mods

    lookupExport (_, avails) =
      concat <$> traverse availExport (CompatGHC.nubAvails avails)

    availExport =
      pollock_availExportItem
        semMod
        warnings
        maps
        dflags

-- Extract the minimal complete definition of a Name, if one exists
minimalDef :: (Monad m) => CompatGHC.Name -> IfM m (Maybe CompatGHC.ClassMinimalDef)
minimalDef n = do
  mty <- lookupName n
  case mty of
    Just (CompatGHC.ATyCon a) ->
      pure . fmap CompatGHC.classMinimalDef $ CompatGHC.tyConClass_maybe a
    _ ->
      pure Nothing

pollock_availExportItem ::
  forall m.
  (Monad m) =>
  CompatGHC.Module -- semantic module
  -> WarningMap
  -> Pollock_Maps
  -> CompatGHC.DynFlags
  -> CompatGHC.AvailInfo
  -> IfM m [Pollock_ExportItem CompatGHC.GhcRn]
pollock_availExportItem
  semMod
  warnings
  (docMap, argMap, declMap)
  dflags =
    declWith
   where
    declWith :: CompatGHC.AvailInfo -> IfM m [Pollock_ExportItem CompatGHC.GhcRn]
    declWith avail' = do
      let t = CompatGHC.availName avail'
      r <- findDecl avail'
      case r of
        ([CompatGHC.L l' (CompatGHC.ValD _ _)], (doc', _)) -> do
          let l = CompatGHC.locA l'
          -- Top-level binding without type signature
          export <- pollock_hiValExportItem dflags t l doc'
          return [export]
        (ds, docs_)
          | decl : _ <- filter (not . CompatGHC.isValD . CompatGHC.unLoc) ds ->
              case decl of
                -- A single signature might refer to many names, but we
                -- create an export item for a single name only.  So we
                -- modify the signature to contain only that single name.
                CompatGHC.L loc (CompatGHC.SigD _ sig) ->
                  -- fromJust is safe since we already checked in guards
                  -- that 't' is a name declared in this declaration.
                  let newDecl = CompatGHC.L loc . CompatGHC.SigD CompatGHC.noExtField . Maybe.fromJust $ filterSigNames (== t) sig
                   in availExportDecl avail' newDecl docs_
                CompatGHC.L loc (CompatGHC.TyClD _ classDecl@(CompatGHC.ClassDecl{})) -> do
                  mdef <- minimalDef t
                  let sig = Maybe.maybeToList $ fmap (CompatGHC.noLocA . CompatGHC.MinimalSig CompatGHC.noAnn CompatGHC.NoSourceText . CompatGHC.noLocA . fmap CompatGHC.noLocA) mdef
                  availExportDecl
                    avail'
                    (CompatGHC.L loc $ CompatGHC.TyClD CompatGHC.noExtField classDecl{CompatGHC.tcdSigs = sig <> CompatGHC.tcdSigs classDecl})
                    docs_
                _ -> availExportDecl avail' decl docs_
        -- Declaration from another package
        ([], _) -> pure [] -- specifically do not care about other packages
        _ -> pure []

    -- Tries 'extractDecl'
    availDecl ::
      CompatGHC.Name -> CompatGHC.LHsDecl CompatGHC.GhcRn -> IfM m (CompatGHC.LHsDecl CompatGHC.GhcRn)
    availDecl declName parentDecl =
      case extractDecl declMap declName parentDecl of
        Right d -> pure d
        Left err -> error $ "availExportItem" <> (show err)

    availExportDecl ::
      CompatGHC.AvailInfo
      -> CompatGHC.LHsDecl CompatGHC.GhcRn
      -> (DocForDecl, [(CompatGHC.Name, DocForDecl)])
      -> IfM m [Pollock_ExportItem CompatGHC.GhcRn]
    availExportDecl avail' decl (doc', subs)
      | CompatGHC.availExportsDecl avail' = do
          extractedDecl <- availDecl (CompatGHC.availName avail') decl

          -- bundled pattern synonyms only make sense if the declaration is
          -- exported (otherwise there would be nothing to bundle to)
          bundledPatSyns <- findBundledPatterns avail'

          pure
            [ Pollock_ExportItemDecl $
                Pollock_ExportDecl
                  { pollock_expItemDecl = restrictTo (fmap fst subs) extractedDecl
                  , pollock_expItemPats = bundledPatSyns
                  , pollock_expItemMbDoc = doc'
                  , pollock_expItemSubDocs = subs
                  }
            ]
      | otherwise = for subs $ \(sub, sub_doc) -> do
          extractedDecl <- availDecl sub decl

          pure
            ( Pollock_ExportItemDecl $
                Pollock_ExportDecl
                  { pollock_expItemDecl = extractedDecl
                  , pollock_expItemPats = []
                  , pollock_expItemMbDoc = sub_doc
                  , pollock_expItemSubDocs = []
                  }
            )

    findDecl ::
      CompatGHC.AvailInfo
      -> IfM m ([CompatGHC.LHsDecl CompatGHC.GhcRn], (DocForDecl, [(CompatGHC.Name, DocForDecl)]))
    findDecl avail'
      | m == semMod =
          case Map.lookup n declMap of
            Just ds -> pure (ds, lookupDocs avail' warnings docMap argMap)
            Nothing
              -- \| is_sig -> do
              --     -- OK, so it wasn't in the local declaration map.  It could
              --     -- have been inherited from a signature.  Reconstitute it
              --     -- from the type.
              --     mb_r <- hiDecl dflags n
              --     case mb_r of
              --       Nothing -> return ([], (noDocForDecl, availNoDocs avail'))
              --       -- TODO: If we try harder, we might be able to find
              --       -- a Haddock!  Look in the Haddocks for each thing in
              --       -- requirementContext (unitState)
              --       Just decl -> return ([decl], (noDocForDecl, availNoDocs avail'))
              | otherwise ->
                  pure ([], (noDocForDecl, availNoDocs avail'))
      -- POL-TODO I _think_ our purposes can simply ignore this, we'll be dealing with a source
      -- module pretty much always anyway.
      -- \| Just iface <- Map.lookup (semToIdMod (moduleUnit thisMod) m) modMap
      -- , Just ds <- Map.lookup n (ifaceDeclMap iface) =
      --     return (ds, lookupDocs avail warnings
      --                       (ifaceDocMap iface)
      --                       (ifaceArgMap iface))
      | otherwise = pure ([], (noDocForDecl, availNoDocs avail'))
     where
      n = CompatGHC.availName avail'
      m = CompatGHC.nameModule n

    findBundledPatterns :: CompatGHC.AvailInfo -> IfM m [(CompatGHC.HsDecl CompatGHC.GhcRn, DocForDecl)]
    findBundledPatterns avail' = do
      patsyns <- for constructor_names $ \name -> do
        mtyThing <- lookupName name
        case mtyThing of
          Just (CompatGHC.AConLike CompatGHC.PatSynCon{}) -> do
            export_items <- declWith (CompatGHC.availFromName name)
            pure
              [ (CompatGHC.unLoc patsyn_decl, patsyn_doc)
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
        filter CompatGHC.isDataConName (availSubordinates avail')

availSubordinates :: CompatGHC.AvailInfo -> [CompatGHC.Name]
availSubordinates = fmap CompatGHC.greNameMangledName . CompatGHC.availSubordinateGreNames

availNoDocs :: CompatGHC.AvailInfo -> [(CompatGHC.Name, DocForDecl)]
availNoDocs avail' =
  zip (availSubordinates avail') (repeat noDocForDecl)

hiDecl ::
  (Monad m) =>
  CompatGHC.DynFlags
  -> CompatGHC.Name
  -> IfM m (Maybe (CompatGHC.LHsDecl CompatGHC.GhcRn))
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
        return (Just $ CompatGHC.noLocA t')
 where
  warnLine x =
     "pollock-bug: " <> (errMsgToString x)
      <> " -- Please report this on the Pollock issue tracker!"
  bugWarn = errMsgFromString . warnLine

{- | A version of 'hiValExportItem' with a bunch of stuff removed in attempt to cut down for the
coverage use case.
-}
pollock_hiValExportItem ::
  (Monad m) =>
  CompatGHC.DynFlags
  -> CompatGHC.Name
  -> CompatGHC.SrcSpan
  -> DocForDecl
  -> IfM m (Pollock_ExportItem CompatGHC.GhcRn)
pollock_hiValExportItem dflags name nLoc doc' = do
  mayDecl <- hiDecl dflags name
  case mayDecl of
    Nothing -> pure Pollock_ExportItemNoDecl
    Just decl -> pure $ Pollock_ExportItemDecl (Pollock_ExportDecl (fixSpan decl) [] doc' [])
 where
  fixSpan (CompatGHC.L (CompatGHC.SrcSpanAnn a l) t) = CompatGHC.L (CompatGHC.SrcSpanAnn a (CompatGHC.combineSrcSpans l nLoc)) t

-- | Lookup docs for a declaration from maps.
lookupDocs ::
  CompatGHC.AvailInfo
  -> WarningMap
  -> DocMap
  -> ArgMap
  -> (DocForDecl, [(CompatGHC.Name, DocForDecl)])
lookupDocs avail' warnings docMap argMap =
  let n = CompatGHC.availName avail'
      lookupArgDoc x = Map.findWithDefault Map.empty x argMap
      lookupDoc name = Documentation (Map.lookup name docMap) (Map.lookup name warnings)
      doc' = (lookupDoc n, lookupArgDoc n)
      subDocs =
        [ (s, (lookupDoc s, lookupArgDoc s))
        | s <- availSubordinates avail'
        ]
   in (doc', subDocs)

pollock_fullModuleContents ::
  (Monad m) =>
  CompatGHC.Module -- semantic module
  -> WarningMap
  -> [CompatGHC.LHsDecl CompatGHC.GhcRn] -- renamed source declarations
  -> Pollock_Maps
  -> CompatGHC.DynFlags
  -> CompatGHC.Avails
  -> IfM m [Pollock_ExportItem CompatGHC.GhcRn]
pollock_fullModuleContents
  semMod
  warnings
  hsdecls
  maps@(_, _, declMap)
  dflags
  avails = do
    let availEnv = CompatGHC.availsToNameEnv (CompatGHC.nubAvails avails)
    (concat . concat)
      `fmap` ( for hsdecls $ \decl -> do
                case decl of
                  (CompatGHC.L _ (CompatGHC.DocD _ (CompatGHC.DocGroup _ _))) ->
                    pure [[pollock_mkExportGroup]]
                  (CompatGHC.L _ (CompatGHC.DocD _ (CompatGHC.DocCommentNamed _ docStr))) ->
                    let
                      doc' = processDocStringParas (CompatGHC.hsDocString . CompatGHC.unLoc $ docStr)
                     in
                      pure [[pollock_mkExportDoc doc']]
                  (CompatGHC.L _ (CompatGHC.ValD _ valDecl))
                    | name : _ <- CompatGHC.collectHsBindBinders CompatGHC.CollNoDictBinders valDecl
                    , Just (CompatGHC.L _ CompatGHC.SigD{} : _) <- filter isSigD <$> Map.lookup name declMap ->
                        pure []
                  _ ->
                    for (CompatGHC.getMainDeclBinder CompatGHC.emptyOccEnv (CompatGHC.unLoc decl)) $ \nm -> do
                      case CompatGHC.lookupNameEnv availEnv nm of
                        Just avail' ->
                          pollock_availExportItem
                            semMod
                            warnings
                            maps
                            dflags
                            avail'
                        Nothing -> pure []
             )
   where
    isSigD (CompatGHC.L _ CompatGHC.SigD{}) = True
    isSigD _ = False

{- | Sometimes the declaration we want to export is not the "main" declaration:
it might be an individual record selector or a class method.  In these
cases we have to extract the required declaration (and somehow cobble
together a type signature for it...).

This function looks through the declarations in this module to try to find
the one with the right name.
-}
extractDecl ::
  (CompatGHC.HasCallStack) =>
  DeclMap
  -- ^ all declarations in the file
  -> CompatGHC.Name
  -- ^ name of the declaration to extract
  -> CompatGHC.LHsDecl CompatGHC.GhcRn
  -- ^ parent declaration
  -> Either ErrMsg (CompatGHC.LHsDecl CompatGHC.GhcRn)
extractDecl declMap name decl
  | name `elem` CompatGHC.getMainDeclBinder CompatGHC.emptyOccEnv (CompatGHC.unLoc decl) = pure decl
  | otherwise =
      case CompatGHC.unLoc decl of
        CompatGHC.TyClD
          _
          d@CompatGHC.ClassDecl
            { CompatGHC.tcdLName = CompatGHC.L _ clsNm
            , CompatGHC.tcdSigs = clsSigs
            , CompatGHC.tcdATs = clsATs
            } ->
            let
              matchesMethod =
                [ lsig
                | lsig <- clsSigs
                , CompatGHC.ClassOpSig _ False _ _ <- pure $ CompatGHC.unLoc lsig
                , -- Note: exclude `default` declarations (see #505)
                name `elem` sigName lsig
                ]

              matchesAssociatedType =
                [ lfam_decl
                | lfam_decl <- clsATs
                , name == CompatGHC.unLoc (CompatGHC.fdLName (CompatGHC.unLoc lfam_decl))
                ]
             in
              -- TODO: document fixity
              case (matchesMethod, matchesAssociatedType) of
                ([s0], _) ->
                  let tyvar_names = CompatGHC.tyClDeclTyVars d
                      CompatGHC.L pos sig = addClassContext clsNm tyvar_names s0
                   in pure (CompatGHC.L pos (CompatGHC.SigD CompatGHC.noExtField sig))
                (_, [CompatGHC.L pos fam_decl]) -> pure (CompatGHC.L pos (CompatGHC.TyClD CompatGHC.noExtField (CompatGHC.FamDecl CompatGHC.noExtField fam_decl)))
                ([], [])
                  | Just (famInstDecl : _) <- Map.lookup name declMap ->
                      extractDecl declMap name famInstDecl
                _ ->
                  Left
                    ( mconcat
                        [ "Ambiguous decl for "
                        , errMsgFromString (CompatGHC.getOccString name)
                        , " in class "
                        , errMsgFromString (CompatGHC.getOccString clsNm)
                        ]
                    )
        CompatGHC.TyClD
          _
          d@CompatGHC.DataDecl
            { CompatGHC.tcdLName = CompatGHC.L _ dataNm
            , CompatGHC.tcdDataDefn = CompatGHC.HsDataDefn{CompatGHC.dd_cons = dataCons}
            } -> do
            let ty_args = lHsQTyVarsToTypes (CompatGHC.tyClDeclTyVars d)
            lsig <-
              if CompatGHC.isDataConName name
                then extractPatternSyn name dataNm ty_args dataCons
                else extractRecSel name dataNm ty_args dataCons
            pure (CompatGHC.SigD CompatGHC.noExtField <$> lsig)
        CompatGHC.TyClD _ CompatGHC.FamDecl{}
          | CompatGHC.isValName name
          , Just (famInst : _) <- Map.lookup name declMap ->
              extractDecl declMap name famInst
        CompatGHC.InstD
          _
          ( CompatGHC.DataFamInstD
              _
              ( CompatGHC.DataFamInstDecl
                  ( CompatGHC.FamEqn
                      { CompatGHC.feqn_tycon = CompatGHC.L _ n
                      , CompatGHC.feqn_pats = tys
                      , CompatGHC.feqn_rhs = defn
                      }
                    )
                )
            ) ->
            if CompatGHC.isDataConName name
              then fmap (CompatGHC.SigD CompatGHC.noExtField) <$> extractPatternSyn name n tys (CompatGHC.dd_cons defn)
              else fmap (CompatGHC.SigD CompatGHC.noExtField) <$> extractRecSel name n tys (CompatGHC.dd_cons defn)
        CompatGHC.InstD _ (CompatGHC.ClsInstD _ CompatGHC.ClsInstDecl{CompatGHC.cid_datafam_insts = insts})
          | CompatGHC.isDataConName name ->
              let matches =
                    [ d' | CompatGHC.L _ d'@(CompatGHC.DataFamInstDecl (CompatGHC.FamEqn{CompatGHC.feqn_rhs = dd})) <- insts, name `elem` fmap CompatGHC.unLoc (concatMap (CompatGHC.getConNames . CompatGHC.unLoc) (CompatGHC.dd_cons dd))
                    ]
               in case matches of
                    [d0] -> extractDecl declMap name (CompatGHC.noLocA (CompatGHC.InstD CompatGHC.noExtField (CompatGHC.DataFamInstD CompatGHC.noExtField d0)))
                    _ -> Left "internal: extractDecl (ClsInstD)"
          | otherwise ->
              let matches =
                    [ d'
                    | CompatGHC.L _ d'@(CompatGHC.DataFamInstDecl d) <-
                        insts
                    , -- , L _ ConDecl { con_details = RecCon rec } <- dd_cons (feqn_rhs d)
                    Just rec <- fmap (CompatGHC.getRecConArgs_maybe . CompatGHC.unLoc) (CompatGHC.dd_cons (CompatGHC.feqn_rhs d))
                    , CompatGHC.ConDeclField{CompatGHC.cd_fld_names = ns} <- fmap CompatGHC.unLoc (CompatGHC.unLoc rec)
                    , CompatGHC.L _ n <- ns
                    , CompatGHC.foExt n == name
                    ]
               in case matches of
                    [d0] -> extractDecl declMap name (CompatGHC.noLocA . CompatGHC.InstD CompatGHC.noExtField $ CompatGHC.DataFamInstD CompatGHC.noExtField d0)
                    _ -> Left "internal: extractDecl (ClsInstD)"
        _ -> Left ("extractDecl: Unhandled decl for " <> String.fromString (CompatGHC.getOccString name))

extractPatternSyn ::
  CompatGHC.Name
  -> CompatGHC.Name
  -> [CompatGHC.LHsTypeArg CompatGHC.GhcRn]
  -> [CompatGHC.LConDecl CompatGHC.GhcRn]
  -> Either ErrMsg (CompatGHC.LSig CompatGHC.GhcRn)
extractPatternSyn nm t tvs cons =
  case filter matches cons of
    [] ->
      Left $ errMsgFromString "A constructor pattern was not found for in a type" -- TODO: Figure out a better way to do this and restore previous error message from the following -- . O.showSDocOneLine O.defaultSDocContext $ O.text "constructor pattern " O.<+> O.ppr nm O.<+> O.text "not found in type" O.<+> O.ppr t
    con : _ -> pure (extract <$> con)
 where
  matches :: CompatGHC.LConDecl CompatGHC.GhcRn -> Bool
  matches (CompatGHC.L _ con) = nm `elem` (CompatGHC.unLoc <$> CompatGHC.getConNames con)
  extract :: CompatGHC.ConDecl CompatGHC.GhcRn -> CompatGHC.Sig CompatGHC.GhcRn
  extract con =
    let args =
          case con of
            CompatGHC.ConDeclH98{CompatGHC.con_args = con_args'} -> case con_args' of
              CompatGHC.PrefixCon _ args' -> fmap CompatGHC.hsScaledThing args'
              CompatGHC.RecCon (CompatGHC.L _ fields) -> CompatGHC.cd_fld_type . CompatGHC.unLoc <$> fields
              CompatGHC.InfixCon arg1 arg2 -> fmap CompatGHC.hsScaledThing [arg1, arg2]
            CompatGHC.ConDeclGADT{CompatGHC.con_g_args = con_args'} -> case con_args' of
              CompatGHC.PrefixConGADT args' -> fmap CompatGHC.hsScaledThing args'
              CompatGHC.RecConGADT (CompatGHC.L _ fields) _ -> CompatGHC.cd_fld_type . CompatGHC.unLoc <$> fields
        typ = longArrow args (data_ty con)
        typ' =
          case con of
            CompatGHC.ConDeclH98{CompatGHC.con_mb_cxt = Just cxt} -> CompatGHC.noLocA (CompatGHC.HsQualTy CompatGHC.noExtField cxt typ)
            _ -> typ
        typ'' = CompatGHC.noLocA (CompatGHC.HsQualTy CompatGHC.noExtField (CompatGHC.noLocA []) typ')
     in CompatGHC.PatSynSig CompatGHC.noAnn [CompatGHC.noLocA nm] (mkEmptySigType typ'')

  longArrow :: [CompatGHC.LHsType CompatGHC.GhcRn] -> CompatGHC.LHsType CompatGHC.GhcRn -> CompatGHC.LHsType CompatGHC.GhcRn
  longArrow inputs output = foldr (\x y -> CompatGHC.noLocA (CompatGHC.HsFunTy CompatGHC.noAnn (CompatGHC.HsUnrestrictedArrow CompatGHC.noHsUniTok) x y)) output inputs

  data_ty con
    | CompatGHC.ConDeclGADT{} <- con = CompatGHC.con_res_ty con
    | otherwise =
        foldl'
          (\x y -> CompatGHC.noLocA (mkAppTyArg x y))
          (CompatGHC.noLocA (CompatGHC.HsTyVar CompatGHC.noAnn CompatGHC.NotPromoted (CompatGHC.noLocA t)))
          tvs
   where
    mkAppTyArg :: CompatGHC.LHsType CompatGHC.GhcRn -> CompatGHC.LHsTypeArg CompatGHC.GhcRn -> CompatGHC.HsType CompatGHC.GhcRn
    mkAppTyArg f (CompatGHC.HsValArg ty) = CompatGHC.HsAppTy CompatGHC.noExtField f ty
    mkAppTyArg f (CompatGHC.HsTypeArg l ki) = CompatGHC.HsAppKindTy l f ki
    mkAppTyArg f (CompatGHC.HsArgPar _) = CompatGHC.HsParTy CompatGHC.noAnn f

extractRecSel ::
  CompatGHC.Name
  -> CompatGHC.Name
  -> [CompatGHC.LHsTypeArg CompatGHC.GhcRn]
  -> [CompatGHC.LConDecl CompatGHC.GhcRn]
  -> Either ErrMsg (CompatGHC.LSig CompatGHC.GhcRn)
extractRecSel _ _ _ [] = Left "extractRecSel: selector not found"
extractRecSel nm t tvs (CompatGHC.L _ con : rest) =
  case CompatGHC.getRecConArgs_maybe con of
    Just (CompatGHC.L _ fields)
      | ((l, CompatGHC.L _ (CompatGHC.ConDeclField _ _nn ty _)) : _) <- matching_fields fields ->
          pure
            ( CompatGHC.L
                (CompatGHC.noAnnSrcSpan l)
                ( CompatGHC.TypeSig
                    CompatGHC.noAnn
                    [CompatGHC.noLocA nm]
                    ( CompatGHC.mkEmptyWildCardBndrs $
                        mkEmptySigType (CompatGHC.noLocA (CompatGHC.HsFunTy CompatGHC.noAnn (CompatGHC.HsUnrestrictedArrow CompatGHC.noHsUniTok) data_ty (CompatGHC.getBangType ty)))
                    )
                )
            )
    _ -> extractRecSel nm t tvs rest
 where
  matching_fields :: [CompatGHC.LConDeclField CompatGHC.GhcRn] -> [(CompatGHC.SrcSpan, CompatGHC.LConDeclField CompatGHC.GhcRn)]
  matching_fields flds =
    [ (CompatGHC.locA l, f) | f@(CompatGHC.L _ (CompatGHC.ConDeclField _ ns _ _)) <- flds, CompatGHC.L l n <- ns, CompatGHC.foExt n == nm
    ]
  data_ty
    -- ResTyGADT _ ty <- con_res con = ty
    | CompatGHC.ConDeclGADT{} <- con = CompatGHC.con_res_ty con
    | otherwise =
        foldl'
          (\x y -> CompatGHC.noLocA (mkAppTyArg x y))
          (CompatGHC.noLocA (CompatGHC.HsTyVar CompatGHC.noAnn CompatGHC.NotPromoted (CompatGHC.noLocA t)))
          tvs
   where
    mkAppTyArg :: CompatGHC.LHsType CompatGHC.GhcRn -> CompatGHC.LHsTypeArg CompatGHC.GhcRn -> CompatGHC.HsType CompatGHC.GhcRn
    mkAppTyArg f (CompatGHC.HsValArg ty) = CompatGHC.HsAppTy CompatGHC.noExtField f ty
    mkAppTyArg f (CompatGHC.HsTypeArg l ki) = CompatGHC.HsAppKindTy l f ki
    mkAppTyArg f (CompatGHC.HsArgPar _) = CompatGHC.HsParTy CompatGHC.noAnn f

-- | Keep export items with docs.
pruneExportItems :: [Pollock_ExportItem CompatGHC.GhcRn] -> [Pollock_ExportItem CompatGHC.GhcRn]
pruneExportItems = filter hasDoc
 where
  hasDoc (Pollock_ExportItemDecl (Pollock_ExportDecl{pollock_expItemMbDoc = (Documentation d _, _)})) = Maybe.isJust d
  hasDoc _ = True

seqList :: [a] -> ()
seqList [] = ()
seqList (x : xs) = x `seq` seqList xs

-- | Find a stand-alone documentation comment by its name.
findNamedDoc ::
  String -> [CompatGHC.HsDecl CompatGHC.GhcRn] -> ErrMsgM (Maybe CompatGHC.HsDocString)
findNamedDoc name = search
 where
  search [] = do
    reportErrorMessage ("Cannot find documentation for: $" <> errMsgFromString name)
    return Nothing
  search (CompatGHC.DocD _ (CompatGHC.DocCommentNamed name' doc') : rest)
    | name == name' = return (Just (CompatGHC.hsDocString . CompatGHC.unLoc $ doc'))
    | otherwise = search rest
  search (_other_decl : rest) = search rest
