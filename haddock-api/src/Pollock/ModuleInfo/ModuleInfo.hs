{- |
Module:  Pollock.ModuleInfo.ModuleInfo
Copyright: (c) Trevis Elser 2023
License:  MIT

Maintainer: trevis@flipstone.com
Stability: experimental
Portability: portable
-}
module Pollock.ModuleInfo.ModuleInfo
  ( ExportItemInfo (..)
  , buildExportItemInfo
  ) where

import qualified Pollock.Documentation as Documentation

data ExportItemInfo = ExportItemInfo
  { hasSince :: !Bool
  , hasCodeBlock :: !Bool
  , hasExample :: !Bool
  , hasProperty :: !Bool
  , hasWarning :: !Bool
  }

buildExportItemInfo :: Documentation.ExportItem -> ExportItemInfo
buildExportItemInfo ei =
  ExportItemInfo
    { hasSince = Documentation.exportItemHasSinceVersion ei
    , hasCodeBlock = Documentation.exportItemHasCodeBlock ei
    , hasExample = Documentation.exportItemHasExample ei
    , hasProperty = Documentation.exportItemHasProperty ei
    , hasWarning = Documentation.exportItemHasWarning ei
    }
