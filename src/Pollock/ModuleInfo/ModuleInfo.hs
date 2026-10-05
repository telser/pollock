{- |
Module:  Pollock.ModuleInfo.ModuleInfo
Copyright: (c) Trevis Elser 2023
License:  MIT
Maintainer: trevis@flipstone.com
Stability: experimental
Portability: portable
-}
module Pollock.ModuleInfo.ModuleInfo
  ( ModuleInfo (..)
  , buildModuleInfo
  ) where


import qualified Pollock.CompatGHC as CompatGHC
import qualified Pollock.Documentation as Documentation
import qualified Pollock.ModuleInfo.ModuleHeader as ModuleHeader

data ModuleInfo = ModuleInfo
  { moduleHeader :: !ModuleHeader.ModuleHeader
  -- ^ The haddock module header
  , haddockableExports :: !Int
  -- ^ How many exported items that could have documentation attached.
  , haddockedExports :: !Int
  -- ^ How many exported items that do have haddock attached.
  , numWithSince :: !Int
  -- ^ How many exported items have a since annotation.
  , numWithCodeBlock :: !Int
  -- ^ How many exported items have a code block included in the documentation.
  , numWithExample :: !Int
  -- ^ How many exported items have an example included in the documentation.
  , numWithProperty :: !Int
  -- ^ How many exported items have a property included in the documentation.
  , numWithWarning :: !Int
  -- ^ How many exported items have a warning attached to them.
  }

buildModuleInfo :: Maybe CompatGHC.HsDocString -> [Documentation.ExportItem] -> ModuleInfo
buildModuleInfo str =
  let
    initialModuleInfo =
      ModuleInfo
        { moduleHeader = ModuleHeader.processModuleHeader str
        , haddockableExports = 0
        , haddockedExports = 0
        , numWithSince = 0
        , numWithCodeBlock = 0
        , numWithExample = 0
        , numWithProperty = 0
        , numWithWarning = 0
        }
   in
    foldr foldExportItemInfo initialModuleInfo

foldExportItemInfo :: Documentation.ExportItem -> ModuleInfo -> ModuleInfo
foldExportItemInfo exportItem moduleInfo =
  let
    bump predicate field =
      if predicate exportItem
        then field moduleInfo + 1
        else field moduleInfo
   in
    moduleInfo
      { haddockableExports =
          haddockableExports moduleInfo + 1
      , haddockedExports =
          bump hasDoc haddockedExports
      , numWithSince =
          bump Documentation.exportItemHasSinceVersion numWithSince
      , numWithCodeBlock =
          bump Documentation.exportItemHasCodeBlock numWithCodeBlock
      , numWithExample =
          bump Documentation.exportItemHasExample numWithExample
      , numWithProperty =
          bump Documentation.exportItemHasProperty numWithProperty
      , numWithWarning =
          bump Documentation.exportItemHasWarning numWithWarning
      }

hasDoc :: Documentation.ExportItem -> Bool
hasDoc
  ( Documentation.ExportItemDecl
      (Documentation.ExportDecl (Documentation.DocumentationForDecl d _ _))
    ) =
    case d >>= Documentation.doc of
      Nothing -> False
      Just _ -> True
hasDoc _ = True
