{- |
Module:  Pollock.Documentation.ExportItem
Copyright: (c) Trevis Elser 2023
License:  MIT

Maintainer: trevis@flipstone.com
Stability: experimental
Portability: portable

-}
module Pollock.Documentation.ExportItem
  ( ExportItem(..)
  , mkExportDoc
  , ExportDecl(..)
  ) where

import qualified Pollock.CompatGHC as CompatGHC
import Pollock.Documentation.DocumentationForDecl
    ( DocumentationForDecl )
import Pollock.Documentation.MetadataAndDoc ( MetaAndDoc )

data ExportItem
  = ExportItemDecl {-# UNPACK #-} !ExportDecl
  | ExportItemDoc !ExportDoc

mkExportDoc :: MetaAndDoc -> ExportItem
mkExportDoc = ExportItemDoc . ExportDoc

data ExportDecl = ExportDecl
  { -- pollock_expItemDecl :: !(CompatGHC.LHsDecl name)
    -- -- ^ A declaration.
    -- ,
    expItemMbDoc :: {-# UNPACK #-} !DocumentationForDecl
  -- ^ Maybe a doc comment, and possibly docs for arguments (if this
  -- decl is a function or type-synonym).
  -- , pollock_expItemSubDocs :: ![(CompatGHC.IdP name, DocumentationForDecl)]
  -- ^ Subordinate names, possibly with documentation.
  }

newtype ExportDoc = ExportDoc MetaAndDoc
