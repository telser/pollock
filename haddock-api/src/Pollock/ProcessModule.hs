{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      :  Pollock.ProcessModule
Copyright   :  (c) Trevis Elser 2023
License     :  MIT

Maintainer  :  trevis@flipstone.com
Stability   :  experimental
-}
module Pollock.ProcessModule
  ( processModule
  ) where

import qualified Control.Applicative as Applicative
import qualified Control.Monad.IO.Class as MIO
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T

import qualified Pollock.CompatGHC as CompatGHC
import qualified Pollock.Documentation as Documentation
import Pollock.ModuleInfo (ModuleHeader (..), processModuleHeader)

processModule ::
  (MIO.MonadIO m) =>
  CompatGHC.TcGblEnv
  -> m (Int, Int, ModuleHeader)
processModule tcGblEnv = do
  let
    -- TyThings that have instances defined in this module
    local_instances :: [CompatGHC.Name]
    local_instances =
      filter
        (CompatGHC.nameIsLocalOrFrom (CompatGHC.tcg_semantic_mod tcGblEnv))
        ( fmap CompatGHC.getName (CompatGHC.tcg_insts tcGblEnv)
            <> fmap CompatGHC.getName (CompatGHC.tcg_fam_insts tcGblEnv)
        )

    exportedNames = exported_names tcGblEnv
    -- Warnings on declarations in this module
    decl_warnings = mkWarningMap (CompatGHC.tcg_warns tcGblEnv) (CompatGHC.tcg_rdr_env tcGblEnv) exportedNames

  -- The docs added via Template Haskell's putDoc
  thDocs@CompatGHC.ExtractedTHDocs{CompatGHC.ethd_mod_header = thMbDocStr} <-
    MIO.liftIO . fmap CompatGHC.extractTHDocs . CompatGHC.readIORef $ CompatGHC.tcg_th_docs tcGblEnv

  -- Process the top-level module header documentation.
  let !info =
        processModuleHeader
          ( fmap CompatGHC.hsDocString thMbDocStr
              Applicative.<|> (CompatGHC.hsDocString . CompatGHC.unLoc <$> CompatGHC.tcg_doc_hdr tcGblEnv)
          )
      ds = decls tcGblEnv
      maps = mkMaps local_instances ds thDocs

      export_items =
        mkExportItems
          (CompatGHC.tcg_semantic_mod tcGblEnv)
          decl_warnings
          (fmap fst ds)
          maps
          (imported_modules tcGblEnv)
          (export_list tcGblEnv)
          (CompatGHC.tcg_exports tcGblEnv)

      pruned_export_items = pruneExportItems export_items
      !haddockable = 1 + length export_items -- module + exports
      !haddocked = (if Maybe.isJust (CompatGHC.tcg_doc_hdr tcGblEnv) then 1 else 0) + length pruned_export_items

  pure (haddockable, haddocked, info)

-- Module imports of the form `import X`. Note that there is
-- a) no qualification and
-- b) no import list
imported_modules :: CompatGHC.TcGblEnv -> Map.Map CompatGHC.ModuleName [CompatGHC.ModuleName]
imported_modules tcGblEnv =
  case export_list tcGblEnv of
    Just _ -> unrestrictedModuleImports (fmap CompatGHC.unLoc (CompatGHC.tcg_rn_imports tcGblEnv))
    Nothing -> Map.empty

decls ::
  CompatGHC.TcGblEnv
  -> [ ( CompatGHC.GenLocated CompatGHC.SrcSpanAnnA (CompatGHC.HsDecl CompatGHC.GhcRn)
       , [CompatGHC.HsDoc CompatGHC.GhcRn]
       )
     ]
decls tcGblEnv =
  case CompatGHC.tcg_rn_decls tcGblEnv of
    Nothing -> do
      --      reportErrorMessage "Warning: Renamed source is not available"
      mempty
    Just dx ->
      CompatGHC.topDecls dx

-- All elements of an explicit export list, if present
export_list :: CompatGHC.TcGblEnv -> Maybe [(CompatGHC.IE CompatGHC.GhcRn, CompatGHC.Avails)]
export_list tcGblEnv =
  case CompatGHC.tcg_rn_exports tcGblEnv of
    Just rn_exports ->
      Just [(ie, avail') | (CompatGHC.L _ ie, avail') <- rn_exports]
    Nothing -> Nothing

exported_names :: CompatGHC.TcGblEnv -> [CompatGHC.Name]
exported_names =
  concatMap CompatGHC.availNames . CompatGHC.tcg_exports

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
unrestrictedModuleImports ::
  [CompatGHC.ImportDecl CompatGHC.GhcRn] -> Map.Map CompatGHC.ModuleName [CompatGHC.ModuleName]
unrestrictedModuleImports idecls =
  Map.map (fmap (CompatGHC.unLoc . CompatGHC.ideclName)) $
    Map.filter (all isInteresting) impModMap
 where
  impModMap =
    Map.fromListWith (++) (concatMap moduleMapping idecls)

  moduleMapping ::
    ( CompatGHC.XRec pass CompatGHC.ModuleName
        ~ CompatGHC.GenLocated l CompatGHC.ModuleName
    ) =>
    CompatGHC.ImportDecl pass
    -> [(CompatGHC.ModuleName, [CompatGHC.ImportDecl pass])]
  moduleMapping idecl =
    concat
      [ [(CompatGHC.unLoc (CompatGHC.ideclName idecl), [idecl])]
      , [ (CompatGHC.unLoc mod_name, [idecl])
        | Just mod_name <- [CompatGHC.ideclAs idecl]
        ]
      ]

  isInteresting ::
    ( CompatGHC.XRec pass [CompatGHC.XRec pass (CompatGHC.IE pass)]
        ~ CompatGHC.GenLocated
            l
            [CompatGHC.XRec pass (CompatGHC.IE pass)]
    ) =>
    CompatGHC.ImportDecl pass
    -> Bool
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
  forall a.
  CompatGHC.Warnings a
  -> CompatGHC.GlobalRdrEnv
  -> [CompatGHC.Name]
  -> WarningMap
mkWarningMap warnings gre = Map.fromList . (fmap . fmap) parseWarning . CompatGHC.processWarnSome warnings gre

parseWarning :: CompatGHC.WarningTxt a -> Documentation.Doc
parseWarning w =
  let
    format :: String -> String -> Documentation.Doc
    format x =
      Documentation.DocWarning
        . Documentation.DocParagraph
        . Documentation.DocAppend (Documentation.DocString x)
        . Documentation.parseText
        . T.pack

    foldMsgs ::
      (Foldable t) =>
      t (CompatGHC.Located (CompatGHC.WithHsDocIdentifiers CompatGHC.StringLiteral pass))
      -> String
    foldMsgs =
      foldMap (CompatGHC.unpackFS . CompatGHC.sl_fs . CompatGHC.hsDocString . CompatGHC.unLoc)

    formatDeprecated ::
      (Foldable t) =>
      t (CompatGHC.Located (CompatGHC.WithHsDocIdentifiers CompatGHC.StringLiteral pass))
      -> Documentation.Doc
    formatDeprecated =
      format "Deprecated: " . foldMsgs

    formatWarning ::
      (Foldable t) =>
      t (CompatGHC.Located (CompatGHC.WithHsDocIdentifiers CompatGHC.StringLiteral pass))
      -> Documentation.Doc
    formatWarning =
      format "Warning: " . foldMsgs
   in
    CompatGHC.mapWarningTxtMsg formatDeprecated formatWarning w

--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------

type Maps =
  ( DocMap
  , ArgMap
  , Map.Map
      CompatGHC.Name
      [CompatGHC.GenLocated CompatGHC.SrcSpanAnnA (CompatGHC.HsDecl CompatGHC.GhcRn)]
  )

type DocMap = Map.Map CompatGHC.Name Documentation.MetaAndDoc
type ArgMap = Map.Map CompatGHC.Name Documentation.FnArgsDoc
type WarningMap = Map.Map CompatGHC.Name Documentation.Doc

{- | Create 'Maps' by looping through the declarations. For each declaration,
find its names, its subordinates, and its doc strings. Process doc strings
into 'Documentation.Doc's.
-}
mkMaps ::
  [CompatGHC.Name]
  -> [(CompatGHC.LHsDecl CompatGHC.GhcRn, [CompatGHC.HsDoc CompatGHC.GhcRn])]
  -> CompatGHC.ExtractedTHDocs
  -- ^ Template Haskell putDoc docs
  -> Maps
mkMaps instances hsdecls thDocs =
  let
    (a, b, c) = unzip3 $ fmap mappings hsdecls
    (th_a, th_b) = thMappings
   in
    ( th_a `Map.union` f' (fmap (CompatGHC.nubByName fst) a)
    , th_b `unionArgMaps` f (filterMapping (not . IM.null) b)
    , f (filterMapping (not . null) c)
    )
 where
  f :: (Ord a, Monoid b) => [[(a, b)]] -> Map.Map a b
  f = Map.fromListWith (<>) . concat

  f' ::
    [[(CompatGHC.Name, Documentation.MetaAndDoc)]] -> Map.Map CompatGHC.Name Documentation.MetaAndDoc
  f' = Map.fromListWith Documentation.metaAndDocAppend . concat

  filterMapping :: (b -> Bool) -> [[(a, b)]] -> [[(a, b)]]
  filterMapping p = fmap (filter (p . snd))

  -- \| Extract the mappings from template haskell.
  -- No DeclMap/InstMap is needed since we already have access to the
  -- doc strings
  thMappings ::
    ( Map.Map CompatGHC.Name Documentation.MetaAndDoc
    , Map.Map CompatGHC.Name (IM.IntMap Documentation.MetaAndDoc)
    )
  thMappings =
    let CompatGHC.ExtractedTHDocs
          _
          declDocs
          argDocs
          instDocs = thDocs
        ds2mdoc :: CompatGHC.HsDoc CompatGHC.GhcRn -> Documentation.MetaAndDoc
        ds2mdoc = Documentation.processDocStringParas . CompatGHC.hsDocString

        declDocs' = fmap ds2mdoc (CompatGHC.nonDetEltUniqMapToMap declDocs)
        argDocs' = fmap (fmap ds2mdoc) (CompatGHC.nonDetEltUniqMapToMap argDocs)
        instDocs' = fmap ds2mdoc (CompatGHC.nonDetEltUniqMapToMap instDocs)
     in (declDocs' <> instDocs', argDocs')

  mappings ::
    (CompatGHC.LHsDecl CompatGHC.GhcRn, [CompatGHC.HsDoc CompatGHC.GhcRn])
    -> ( [(CompatGHC.Name, Documentation.MetaAndDoc)]
       , [(CompatGHC.Name, IM.IntMap Documentation.MetaAndDoc)]
       , [(CompatGHC.Name, [CompatGHC.LHsDecl CompatGHC.GhcRn])]
       )
  mappings (ldecl@(CompatGHC.L (CompatGHC.SrcSpanAnn _ (CompatGHC.RealSrcSpan l _)) decl), hs_docStrs) =
    let docStrs = fmap CompatGHC.hsDocString hs_docStrs
        declDoc ::
          [CompatGHC.HsDocString]
          -> IM.IntMap CompatGHC.HsDocString
          -> (Maybe Documentation.MetaAndDoc, IM.IntMap Documentation.MetaAndDoc)
        declDoc strs m =
          (Documentation.processDocStrings strs, fmap Documentation.processDocStringParas m)

        (doc', args) = declDoc docStrs (fmap CompatGHC.hsDocString (CompatGHC.declTypeDocs decl))

        subs :: [(CompatGHC.Name, [CompatGHC.HsDocString], IM.IntMap CompatGHC.HsDocString)]
        subs =
          fmap (\(n, ds, im) -> (n, fmap CompatGHC.hsDocString ds, fmap CompatGHC.hsDocString im)) $
            CompatGHC.subordinates CompatGHC.emptyOccEnv instanceMap decl

        fn ::
          (a, [CompatGHC.HsDocString], IM.IntMap CompatGHC.HsDocString)
          -> (Maybe Documentation.MetaAndDoc, IM.IntMap Documentation.MetaAndDoc)
        fn (_, strs, m) = declDoc strs m
        (subDocs, subArgs) = unzip $ fmap fn subs

        ns = names l decl
        fst3 :: (a, b, c) -> a
        fst3 (a, _, _) = a
        subNs = fmap fst3 subs
        dm = [(n, d) | (n, Just d) <- zip ns (repeat doc') <> zip subNs subDocs]
        am = [(n, args) | n <- ns] <> zip subNs subArgs
        cm = [(n, [ldecl]) | n <- ns <> subNs]
     in seqList ns `seq`
          seqList subNs `seq`
            doc' `seq`
              seqList subDocs `seq`
                seqList subArgs `seq`
                  (dm, am, cm)
  mappings (CompatGHC.L (CompatGHC.SrcSpanAnn _ (CompatGHC.UnhelpfulSpan _)) _, _) = mempty

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
mkExportItems ::
  CompatGHC.Module -- semantic module
  -> WarningMap
  -> [CompatGHC.LHsDecl CompatGHC.GhcRn] -- renamed source declarations
  -> Maps
  -> Map.Map CompatGHC.ModuleName [CompatGHC.ModuleName]
  -> Maybe [(CompatGHC.IE CompatGHC.GhcRn, CompatGHC.Avails)]
  -> CompatGHC.Avails -- exported stuff from this module
  -> [Documentation.ExportItem]
mkExportItems
  semMod
  warnings
  hsdecls
  maps
  unrestricted_imp_mods
  exportList
  allExports =
    case exportList of
      Nothing ->
        fullModuleContents
          semMod
          warnings
          hsdecls
          maps
          allExports
      Just exports -> concat $ mapM lookupExport exports
   where
    lookupExport ::
      ( CompatGHC.XRec pass CompatGHC.ModuleName
          ~ CompatGHC.GenLocated l CompatGHC.ModuleName
      ) =>
      (CompatGHC.IE pass, [CompatGHC.AvailInfo])
      -> [Documentation.ExportItem]
    lookupExport (CompatGHC.IEGroup _ _ _, _) =
      mempty
    lookupExport (CompatGHC.IEDoc _ docStr, _) =
      [ Documentation.mkExportDoc . Documentation.processDocStringParas . CompatGHC.hsDocString $
          CompatGHC.unLoc docStr
      ]
    lookupExport (CompatGHC.IEDocNamed _ _, _) =
      -- FIXME: If we have some named docs then that isn't really an export of some code to keep
      -- track of for coverage or other analysis. Make sure we don't need to restore this for
      -- something though.
      mempty
    -- liftErrMsg $
    --   findNamedDoc str (fmap CompatGHC.unLoc hsdecls) >>= \case
    --     Nothing -> pure []
    --     Just docStr ->
    --       pure [pollock_mkExportDoc $ processDocStringParas docStr]
    lookupExport (CompatGHC.IEModuleContents _ (CompatGHC.L _ mod_name), _)
      -- only consider exporting a module if we are sure we
      -- are really exporting the whole module and not some
      -- subset. We also look through module aliases here.
      | Just mods <- Map.lookup mod_name unrestricted_imp_mods
      , not (null mods) =
          mempty
    -- FIXME Can we get away with completely ignoring module exports like this?
    -- concat <$> traverse (moduleExport thisMod dflags modMap instIfaceMap) mods

    lookupExport (_, avails) =
      concatMap availExport (CompatGHC.nubAvails avails)

    availExport =
      availExportItem
        semMod
        warnings
        maps

availExportItem ::
  CompatGHC.Module -- semantic module
  -> WarningMap
  -> Maps
  -> CompatGHC.AvailInfo
  -> [Documentation.ExportItem]
availExportItem
  semMod
  warnings
  (docMap, argMap, declMap) =
    declWith
   where
    declWith :: CompatGHC.AvailInfo -> [Documentation.ExportItem]
    declWith avail' =
      case findDecl avail' of
        ([CompatGHC.L _ (CompatGHC.ValD _ _)], (doc', _)) ->
          -- Top-level binding without type signature
          [Documentation.ExportItemDecl $ Documentation.ExportDecl doc']
        (ds, docs_)
          | decl : _ <- filter (not . CompatGHC.isValD . CompatGHC.unLoc) ds ->
              case decl of
                CompatGHC.L _ (CompatGHC.SigD _ _) ->
                  availExportDecl avail' docs_
                CompatGHC.L _loc (CompatGHC.TyClD _ _classDecl@(CompatGHC.ClassDecl{})) ->
                  availExportDecl
                    avail'
                    docs_
                _ -> availExportDecl avail' docs_
        -- Declaration from another package
        ([], _) -> mempty -- specifically do not care about other packages
        _ -> mempty

    availExportDecl ::
      CompatGHC.AvailInfo
      -> (Documentation.DocumentationForDecl, [(CompatGHC.Name, Documentation.DocumentationForDecl)])
      -> [Documentation.ExportItem]
    availExportDecl avail' (doc', subs)
      | CompatGHC.availExportsDecl avail' =
          [ Documentation.ExportItemDecl $
              Documentation.ExportDecl
                { Documentation.expItemMbDoc = doc'
                }
          ]
      | otherwise = fmap (Documentation.ExportItemDecl . Documentation.ExportDecl . snd) subs

    findDecl ::
      CompatGHC.AvailInfo
      -> ( [CompatGHC.LHsDecl CompatGHC.GhcRn]
         , (Documentation.DocumentationForDecl, [(CompatGHC.Name, Documentation.DocumentationForDecl)])
         )
    findDecl avail'
      | m == semMod =
          case Map.lookup n declMap of
            Just ds -> (ds, lookupDocs avail' warnings docMap argMap)
            Nothing
              -- \| is_sig -> do
              --     -- OK, so it wasn't in the local declaration map.  It could
              --     -- have been inherited from a signature.  Reconstitute it
              --     -- from the type.
              --     mb_r <- hiDecl dflags n
              --     case mb_r of
              --       Nothing -> return ([], (noDocumentationForDecl, availNoDocs avail'))
              --       -- TODO: If we try harder, we might be able to find
              --       -- a Haddock!  Look in the Haddocks for each thing in
              --       -- requirementContext (unitState)
              --       Just decl -> return ([decl], (noDocumentationForDecl, availNoDocs avail'))
              | otherwise ->
                  (mempty, (Documentation.noDocumentationForDecl, availNoDocs avail'))
      -- POL-TODO I _think_ our purposes can simply ignore this, we'll be dealing with a source
      -- module pretty much always anyway.
      -- \| Just iface <- Map.lookup (semToIdMod (moduleUnit thisMod) m) modMap
      -- , Just ds <- Map.lookup n (ifaceDeclMap iface) =
      --     return (ds, lookupDocs avail warnings
      --                       (ifaceDocMap iface)
      --                       (ifaceArgMap iface))
      | otherwise = (mempty, (Documentation.noDocumentationForDecl, availNoDocs avail'))
     where
      n = CompatGHC.availName avail'
      m = CompatGHC.nameModule n

availNoDocs :: CompatGHC.AvailInfo -> [(CompatGHC.Name, Documentation.DocumentationForDecl)]
availNoDocs =
  fmap (,Documentation.noDocumentationForDecl) . CompatGHC.availSubordinateNames

-- | Lookup docs for a declaration from maps.
lookupDocs ::
  CompatGHC.AvailInfo
  -> WarningMap
  -> DocMap
  -> ArgMap
  -> (Documentation.DocumentationForDecl, [(CompatGHC.Name, Documentation.DocumentationForDecl)])
lookupDocs avail' warnings docMap argMap =
  let n = CompatGHC.availName avail'
      lookupDoc name =
        Documentation.DocumentationForDecl
          (Map.lookup name docMap)
          (Map.lookup name warnings)
          (Map.findWithDefault IM.empty name argMap)
      subDocs =
        fmap (\x -> (x, lookupDoc x)) $ CompatGHC.availSubordinateNames avail'
   in (lookupDoc n, subDocs)

fullModuleContents ::
  CompatGHC.Module -- semantic module
  -> WarningMap
  -> [CompatGHC.LHsDecl CompatGHC.GhcRn] -- renamed source declarations
  -> Maps
  -> CompatGHC.Avails
  -> [Documentation.ExportItem]
fullModuleContents
  semMod
  warnings
  hsdecls
  maps@(_, _, declMap)
  avails =
    let availEnv = CompatGHC.availsToNameEnv (CompatGHC.nubAvails avails)
        fn :: CompatGHC.GenLocated l (CompatGHC.HsDecl CompatGHC.GhcRn) -> [[Documentation.ExportItem]]
        fn decl =
          case decl of
            (CompatGHC.L _ (CompatGHC.DocD _ (CompatGHC.DocGroup _ _))) ->
              mempty
            (CompatGHC.L _ (CompatGHC.DocD _ (CompatGHC.DocCommentNamed _ docStr))) ->
              let
                doc' = Documentation.processDocStringParas (CompatGHC.hsDocString . CompatGHC.unLoc $ docStr)
               in
                [[Documentation.mkExportDoc doc']]
            (CompatGHC.L _ (CompatGHC.ValD _ valDecl))
              | name : _ <- CompatGHC.collectHsBindBinders CompatGHC.CollNoDictBinders valDecl
              , Just (CompatGHC.L _ CompatGHC.SigD{} : _) <- filter isSigD <$> Map.lookup name declMap ->
                  mempty
            _ ->
              let
                gn nm =
                  case CompatGHC.lookupNameEnv availEnv nm of
                    Just avail' ->
                      availExportItem
                        semMod
                        warnings
                        maps
                        avail'
                    Nothing -> mempty
               in
                fmap gn (CompatGHC.getMainDeclBinder CompatGHC.emptyOccEnv (CompatGHC.unLoc decl))
     in concat (concatMap fn hsdecls)
   where
    isSigD :: CompatGHC.GenLocated l (CompatGHC.HsDecl p) -> Bool
    isSigD (CompatGHC.L _ CompatGHC.SigD{}) = True
    isSigD _ = False

-- | Keep export items with docs.
pruneExportItems :: [Documentation.ExportItem] -> [Documentation.ExportItem]
pruneExportItems = filter hasDoc
 where
  hasDoc
    ( Documentation.ExportItemDecl
        (Documentation.ExportDecl{Documentation.expItemMbDoc = Documentation.DocumentationForDecl d _ _})
      ) = Maybe.isJust d
  hasDoc _ = True

seqList :: [a] -> ()
seqList [] = ()
seqList (x : xs) = x `seq` seqList xs
