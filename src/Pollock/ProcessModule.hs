{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module: Pollock.ProcessModule
Copyright: (c) Trevis Elser 2023
License: MIT

Maintainer: trevis@flipstone.com
Stability: experimental
-}
module Pollock.ProcessModule
  ( processModule
  ) where

import qualified Control.Applicative as Applicative
import qualified Control.Monad.IO.Class as MIO
import qualified Data.Bifunctor as Bifunctor
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T

import qualified Pollock.CompatGHC as CompatGHC
import qualified Pollock.Documentation as Documentation
import Pollock.ModuleInfo (ModuleInfo, buildModuleInfo)

processModule ::
  (MIO.MonadIO m) =>
  CompatGHC.TcGblEnv
  -> m ModuleInfo
processModule tcGblEnv = do
  let
    localInstances :: [CompatGHC.Name]
    localInstances =
      filter
        (CompatGHC.nameIsLocalOrFrom (CompatGHC.tcg_semantic_mod tcGblEnv))
        ( fmap CompatGHC.getName (CompatGHC.tcg_insts tcGblEnv)
            <> fmap CompatGHC.getName (CompatGHC.tcg_fam_insts tcGblEnv)
        )

    tcgExports = CompatGHC.tcg_exports tcGblEnv
    exportedNames = concatMap CompatGHC.availNames tcgExports
    -- Warnings on declarations in this module
    decl_warnings = mkWarningMap (CompatGHC.tcg_warns tcGblEnv) (CompatGHC.tcg_rdr_env tcGblEnv) exportedNames

  -- The docs added via Template Haskell's putDoc
  thDocs <-
    MIO.liftIO . fmap CompatGHC.extractTHDocs . CompatGHC.readIORef $ CompatGHC.tcg_th_docs tcGblEnv

  -- Process the top-level module header documentation.
  let mbHeaderStr =
        fmap CompatGHC.hsDocString (CompatGHC.ethd_mod_header thDocs)
          Applicative.<|> CompatGHC.getHeaderInfo tcGblEnv

      decls = maybe mempty CompatGHC.topDecls $ CompatGHC.tcg_rn_decls tcGblEnv
      maps = mkMaps localInstances decls thDocs

      exportItems =
        mkExportItems
          (CompatGHC.tcg_semantic_mod tcGblEnv)
          decl_warnings
          (fmap fst decls)
          maps
          (importedModules tcGblEnv)
          (fullExplicitExportList tcGblEnv)
          tcgExports

  pure $ buildModuleInfo mbHeaderStr exportItems

-- Module imports of the form `import X`. Note that there is
-- a) no qualification and
-- b) no import list
importedModules :: CompatGHC.TcGblEnv -> Map.Map CompatGHC.ModuleName [CompatGHC.ModuleName]
importedModules tcGblEnv =
  -- If rn_exports aren't available then we know renamed source overall is not available and can
  -- short circuit here.
  case fullExplicitExportList tcGblEnv of
    Just _ -> unrestrictedModuleImports (fmap CompatGHC.unLoc (CompatGHC.tcg_rn_imports tcGblEnv))
    Nothing -> Map.empty

-- All elements of an explicit export list, if present
fullExplicitExportList ::
  CompatGHC.TcGblEnv -> Maybe [(CompatGHC.IE CompatGHC.GhcRn, CompatGHC.Avails)]
fullExplicitExportList =
  (fmap . fmap) unLocFirst . CompatGHC.tcg_rn_exports

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
  fmap (fmap (CompatGHC.unLoc . CompatGHC.ideclName)) $
    Map.filter (all isInteresting) impModMap
 where
  impModMap =
    Map.fromListWith (<>) (concatMap moduleMapping idecls)

  moduleMapping ::
    CompatGHC.ImportDecl CompatGHC.GhcRn
    -> [(CompatGHC.ModuleName, [CompatGHC.ImportDecl CompatGHC.GhcRn])]
  moduleMapping idecl =
    pure (CompatGHC.unLoc (CompatGHC.ideclName idecl), pure idecl)
      <> ( case CompatGHC.ideclAs idecl of
            Just modName ->
              pure (CompatGHC.unLoc modName, pure idecl)
            _ ->
              mempty
         )

  isInteresting :: CompatGHC.ImportDecl CompatGHC.GhcRn -> Bool
  isInteresting idecl =
    case CompatGHC.ideclImportList idecl of
      -- i) no subset selected
      Nothing -> True
      -- ii) an import with a hiding clause
      -- without any names
      Just (CompatGHC.EverythingBut, CompatGHC.L _ []) -> True
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
mkWarningMap warnings gre =
  Map.fromList . (fmap . fmap) parseWarning . CompatGHC.processWarnSome warnings gre

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

    foldMsgs =
      foldMap (CompatGHC.stringLiteralToString . CompatGHC.hsDocString . CompatGHC.unLoc)

    formatDeprecated =
      format "Deprecated: " . foldMsgs

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
      [CompatGHC.HsDecl CompatGHC.GhcRn]
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
mkMaps
  instances
  hsdecls
  (CompatGHC.ExtractedTHDocs _ declDocs argDocs instDocs) =
    let
      thProcessedArgDocs = fmap (fmap mkMetaAndDoc) (CompatGHC.nonDetEltUniqMapToMap argDocs)
      thProcessedDeclDocs = fmap mkMetaAndDoc (CompatGHC.nonDetEltUniqMapToMap declDocs)
      thProcessedInstDocs = fmap mkMetaAndDoc (CompatGHC.nonDetEltUniqMapToMap instDocs)
      thDeclAndInstDocs = thProcessedDeclDocs <> thProcessedInstDocs
      (declDocLists, declArgLists, declLists) = unzip3 $ fmap (nonTHMappings instances) hsdecls
     in
      ( Map.union thDeclAndInstDocs $ buildDocMap declDocLists
      , unionArgMaps thProcessedArgDocs $ buildMapWithNotNullValues IM.null declArgLists
      , buildMapWithNotNullValues null declLists
      )

nonTHMappings ::
  [CompatGHC.Name]
  -> (CompatGHC.LHsDecl CompatGHC.GhcRn, [CompatGHC.HsDoc CompatGHC.GhcRn])
  -> ( [(CompatGHC.Name, Documentation.MetaAndDoc)]
     , [(CompatGHC.Name, IM.IntMap Documentation.MetaAndDoc)]
     , [(CompatGHC.Name, [CompatGHC.HsDecl CompatGHC.GhcRn])]
     )
nonTHMappings instances (CompatGHC.L _ decl, hs_docStrs) =
  let args :: IM.IntMap Documentation.MetaAndDoc
      args =
        fmap mkMetaAndDoc (CompatGHC.declTypeDocs decl)

      instanceMap :: Map.Map CompatGHC.RealSrcSpan CompatGHC.Name
      instanceMap =
        Map.fromList $ foldr instanceFoldFn mempty instances

      (subNs, subDocs, subArgs) =
        unzip3 . fmap processSubordinates $
          CompatGHC.subordinates CompatGHC.emptyOccEnv instanceMap decl

      names = getAssociatedNames decl instanceMap

      docMapping =
        Maybe.catMaybes subDocs
          <> case processDocStrings hs_docStrs of
            Just doc ->
              fmap (\x -> (x, doc)) names
            Nothing ->
              mempty
      argMapping = fmap (\x -> (x, args)) names <> subArgs

      declMapping :: [(CompatGHC.Name, [CompatGHC.HsDecl CompatGHC.GhcRn])]
      declMapping = fmap (\x -> (x, pure decl)) $ names <> subNs
   in (docMapping, argMapping, declMapping)

processSubordinates ::
  (a, [CompatGHC.HsDoc CompatGHC.GhcRn], IM.IntMap (CompatGHC.HsDoc CompatGHC.GhcRn))
  -> (a, Maybe (a, Documentation.MetaAndDoc), (a, IM.IntMap Documentation.MetaAndDoc))
processSubordinates (name, docStrs', docStrMap) =
  (name, maybeSnd (name, processDocStrings docStrs'), (name, fmap mkMetaAndDoc docStrMap))

instanceFoldFn ::
  CompatGHC.Name
  -> [(CompatGHC.RealSrcSpan, CompatGHC.Name)]
  -> [(CompatGHC.RealSrcSpan, CompatGHC.Name)]
instanceFoldFn n accum =
  case CompatGHC.getSrcSpan n of
    CompatGHC.RealSrcSpan l _ ->
      (l, n) : accum
    _ -> accum

getAssociatedNames ::
  CompatGHC.HsDecl CompatGHC.GhcRn
  -> Map.Map CompatGHC.RealSrcSpan CompatGHC.Name
  -> [CompatGHC.Name]
getAssociatedNames (CompatGHC.InstD _ d) instanceMap =
  let
    loc =
      case d of
        -- The CoAx's loc is the whole line, but only for TFs. The
        -- workaround is to dig into the family instance declaration and
        -- get the identifier with the right location.
        CompatGHC.TyFamInstD _ (CompatGHC.TyFamInstDecl _ d') -> CompatGHC.getLocA (CompatGHC.feqn_tycon d')
        _ -> CompatGHC.getInstLoc d
   in
    Maybe.maybeToList (CompatGHC.lookupSrcSpan loc instanceMap) -- See note [2].
getAssociatedNames decl _ =
  CompatGHC.getMainDeclBinder CompatGHC.emptyOccEnv decl

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
  go n newArgMap acc =
    case Map.lookup n acc of
      Just oldArgMap ->
        Map.insert n (newArgMap `IM.union` oldArgMap) acc
      Nothing -> Map.insert n newArgMap acc

-- Note [2]:
------------
-- We relate ClsInsts to InstDecls and DerivDecls using the SrcSpans buried
-- inside them. That should work for normal user-written instances (from
-- looking at GHC sources). We can assume that commented instances are
-- user-written. This lets us relate Names (from ClsInsts) to comments
-- (associated with InstDecls and DerivDecls).

buildDocMap ::
  (Foldable t) =>
  t [(CompatGHC.Name, Documentation.MetaAndDoc)]
  -> Map.Map CompatGHC.Name Documentation.MetaAndDoc
buildDocMap =
  fromListWithAndFilter Documentation.metaAndDocAppend (CompatGHC.nubByName fst)

fromListWithAndFilter ::
  (Ord k, Foldable t) =>
  (a -> a -> a)
  -> (b -> [(k, a)])
  -> t b
  -> Map.Map k a
fromListWithAndFilter appendFn filterFn =
  Map.fromListWith appendFn . concatMap filterFn

buildMapWithNotNullValues ::
  (Semigroup b) =>
  (b -> Bool)
  -> [[(CompatGHC.Name, b)]]
  -> Map.Map CompatGHC.Name b
buildMapWithNotNullValues nullFn =
  fromListWithAndFilter (<>) (filter (not . nullFn . snd))

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
  -> Map.Map CompatGHC.ModuleName [CompatGHC.ModuleName] -- imported modules
  -> Maybe [(CompatGHC.IE CompatGHC.GhcRn, CompatGHC.Avails)]
  -> CompatGHC.Avails -- exported stuff from this module
  -> [Documentation.ExportItem]
mkExportItems semMod warnings hsdecls maps unrestricted_imp_mods exportList allExports =
  case exportList of
    Nothing ->
      fullModuleContents
        semMod
        warnings
        hsdecls
        maps
        allExports
    Just exports -> concat $ traverse lookupExport exports
 where
  lookupExport ::
    (CompatGHC.IE CompatGHC.GhcRn, [CompatGHC.AvailInfo])
    -> [Documentation.ExportItem]
  lookupExport (CompatGHC.IEGroup{}, _) =
    mempty
  lookupExport (CompatGHC.IEDoc _ docStr, _) =
    pure . Documentation.mkExportDoc . mkMetaAndDoc $ CompatGHC.unLoc docStr
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
    availExportItem semMod warnings maps

availExportItem ::
  CompatGHC.Module -- semantic module
  -> WarningMap
  -> Maps
  -> CompatGHC.AvailInfo
  -> [Documentation.ExportItem]
availExportItem semMod warnings (docMap, argMap, declMap) avail =
  let
    n = CompatGHC.availName avail
   in
    if CompatGHC.nameModule n == semMod
      then case Map.lookup n declMap of
        Just [CompatGHC.ValD _ _] ->
          pure . Documentation.ExportItemDecl . Documentation.ExportDecl . fst $
            lookupDocs avail warnings docMap argMap
        Just ds ->
          case filter (not . CompatGHC.isValD) ds of
            [_] ->
              availExportDecl avail $ lookupDocs avail warnings docMap argMap
            _ ->
              mempty
        Nothing ->
          mempty
      else mempty

availExportDecl ::
  CompatGHC.AvailInfo
  -> (Documentation.DocumentationForDecl, [(CompatGHC.Name, Documentation.DocumentationForDecl)])
  -> [Documentation.ExportItem]
availExportDecl avail (doc, subs) =
  if CompatGHC.availExportsDecl avail
    then pure . Documentation.ExportItemDecl $ Documentation.ExportDecl doc
    else fmap (Documentation.ExportItemDecl . Documentation.ExportDecl . snd) subs

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
fullModuleContents semMod warnings hsdecls maps@(_, _, declMap) avails =
  let availEnv = CompatGHC.availsToNameEnv (CompatGHC.nubAvails avails)
      fn :: CompatGHC.HsDecl CompatGHC.GhcRn -> [Documentation.ExportItem]
      fn decl =
        case decl of
          (CompatGHC.DocD _ (CompatGHC.DocGroup _ _)) ->
            mempty
          (CompatGHC.DocD _ (CompatGHC.DocCommentNamed _ docStr)) ->
            let
              doc' = mkMetaAndDoc $ CompatGHC.unLoc docStr
             in
              pure $ Documentation.mkExportDoc doc'
          (CompatGHC.ValD _ valDecl)
            | name : _ <- CompatGHC.collectHsBindBinders CompatGHC.CollNoDictBinders valDecl
            , Just (CompatGHC.SigD{} : _) <- filter isSigD <$> Map.lookup name declMap ->
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
              concatMap gn (CompatGHC.getMainDeclBinder CompatGHC.emptyOccEnv decl)
   in concatMap (fn . CompatGHC.unLoc) hsdecls

isSigD :: CompatGHC.HsDecl p -> Bool
isSigD (CompatGHC.SigD{}) = True
isSigD _ = False

mkMetaAndDoc :: CompatGHC.HsDoc CompatGHC.GhcRn -> Documentation.MetaAndDoc
mkMetaAndDoc = Documentation.processDocStringParas . CompatGHC.hsDocString

processDocStrings :: [CompatGHC.HsDoc CompatGHC.GhcRn] -> Maybe Documentation.MetaAndDoc
processDocStrings = Documentation.processDocStrings . fmap CompatGHC.hsDocString

unLocFirst :: (Bifunctor.Bifunctor bf) => bf (CompatGHC.GenLocated l b) c -> bf b c
unLocFirst =
  Bifunctor.first CompatGHC.unLoc

maybeSnd :: (a, Maybe b) -> Maybe (a, b)
maybeSnd (a, Just b) = Just (a, b)
maybeSnd (_, Nothing) = Nothing
