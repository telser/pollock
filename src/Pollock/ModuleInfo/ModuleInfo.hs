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

import qualified Data.Maybe as Maybe

import qualified Pollock.CompatGHC as CompatGHC
import qualified Pollock.Documentation as Documentation
import Pollock.ModuleInfo.ModuleHeader

data ModuleInfo = ModuleInfo
  { moduleHeader :: !ModuleHeader
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
        { moduleHeader = processModuleHeader str
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
  if hasDoc exportItem
    then
      moduleInfo
        { haddockableExports =
            haddockableExports moduleInfo + 1
        , haddockedExports =
            haddockedExports moduleInfo + 1
        , numWithSince =
            if Documentation.exportItemHasSinceVersion exportItem
              then numWithSince moduleInfo + 1
              else numWithSince moduleInfo
        , numWithCodeBlock =
            if Documentation.exportItemHasCodeBlock exportItem
              then numWithCodeBlock moduleInfo + 1
              else numWithCodeBlock moduleInfo
        , numWithExample =
            if Documentation.exportItemHasExample exportItem
              then numWithExample moduleInfo + 1
              else numWithExample moduleInfo
        , numWithProperty =
            if Documentation.exportItemHasProperty exportItem
              then numWithProperty moduleInfo + 1
              else numWithProperty moduleInfo
        , numWithWarning =
            if Documentation.exportItemHasWarning exportItem
              then numWithWarning moduleInfo + 1
              else numWithWarning moduleInfo
        }
    else -- With no docs we _only_ had an export, but nothing else.

      moduleInfo
        { haddockableExports =
            haddockableExports moduleInfo + 1
        }

hasDoc :: Documentation.ExportItem -> Bool
hasDoc
  ( Documentation.ExportItemDecl
      (Documentation.ExportDecl (Documentation.DocumentationForDecl d _ _))
    ) = Maybe.isJust d
hasDoc _ = True
