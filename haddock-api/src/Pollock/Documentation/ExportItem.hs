{- |
Module:  Pollock.Documentation.ExportItem
Copyright: (c) Trevis Elser 2023
License:  MIT

Maintainer: trevis@flipstone.com
Stability: experimental
Portability: portable
-}
module Pollock.Documentation.ExportItem
  ( ExportItem (..)
  , mkExportDoc
  , ExportDecl (..)
  , exportItemHasSinceVersion
  , exportItemHasCodeBlock
  , exportItemHasExample
  , exportItemHasProperty
  , exportItemHasWarning
  ) where

import Pollock.Documentation.Doc
  (docHasWarning
  , docHasProperty
  , docHasCodeBlock
  , docHasExamples
  )
import Pollock.Documentation.DocumentationForDecl
  ( DocumentationForDecl
  , documentationDoc
  )
import Pollock.Documentation.Metadata (metadataHasSinceVersion)
import Pollock.Documentation.MetadataAndDoc (MetaAndDoc, meta, doc)

data ExportItem
  = ExportItemDecl {-# UNPACK #-} !ExportDecl
  | ExportItemDoc !MetaAndDoc

mkExportDoc :: MetaAndDoc -> ExportItem
mkExportDoc = ExportItemDoc

newtype ExportDecl = ExportDecl
  { expItemMbDoc :: DocumentationForDecl
  }

exportItemHasSinceVersion :: ExportItem -> Bool
exportItemHasSinceVersion (ExportItemDoc md) =
  metadataHasSinceVersion $ meta md
exportItemHasSinceVersion (ExportItemDecl decl) =
  case documentationDoc $ expItemMbDoc decl of
    Nothing -> False
    Just md -> metadataHasSinceVersion $ meta md

exportItemHasWarning :: ExportItem -> Bool
exportItemHasWarning (ExportItemDoc md) =
  docHasWarning $ doc md
exportItemHasWarning (ExportItemDecl decl) =
  case documentationDoc $ expItemMbDoc decl of
    Nothing -> False
    Just md -> docHasWarning $ doc md

exportItemHasProperty :: ExportItem -> Bool
exportItemHasProperty (ExportItemDoc md) =
  docHasProperty $ doc md
exportItemHasProperty (ExportItemDecl decl) =
  case documentationDoc $ expItemMbDoc decl of
    Nothing -> False
    Just md -> docHasProperty $ doc md

exportItemHasExample :: ExportItem -> Bool
exportItemHasExample (ExportItemDoc md) =
  docHasExamples $ doc md
exportItemHasExample (ExportItemDecl decl) =
  case documentationDoc $ expItemMbDoc decl of
    Nothing -> False
    Just md -> docHasExamples $ doc md

exportItemHasCodeBlock :: ExportItem -> Bool
exportItemHasCodeBlock (ExportItemDoc md) =
  docHasCodeBlock $ doc md
exportItemHasCodeBlock (ExportItemDecl decl) =
  case documentationDoc $ expItemMbDoc decl of
    Nothing -> False
    Just md -> docHasCodeBlock $ doc md
