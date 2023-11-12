{- |
Module:  Pollock.Documentation.DocumentationForDecl
Copyright:  (c) Trevis Elser 2023
License:  MIT

Maintainer:  trevis@flipstone.com
Stability:  experimental
Portability:  non-portable

Type and functionality related to documentation for a declaration.

-}
module Pollock.Documentation.DocumentationForDecl
  ( DocumentationForDecl(..)
  , FnArgsDoc
  , noDocumentationForDecl
  )
  where

import qualified Data.IntMap.Strict as IM

import Pollock.Documentation.Doc ( Doc )
import Pollock.Documentation.MetadataAndDoc ( MetaAndDoc )

-- | Represent the documentation for a declaration, optionally including a warning as well as any function arguments.
data DocumentationForDecl = DocumentationForDecl
  { documentationDoc ::  !(Maybe MetaAndDoc)
  , documentationWarning :: !(Maybe Doc)
  , documentationFunctionArgDoc :: !FnArgsDoc
  }

{- | Arguments are indexed by Int, zero-based from the left.
-}
type FnArgsDoc = IM.IntMap MetaAndDoc

noDocumentationForDecl :: DocumentationForDecl
noDocumentationForDecl = DocumentationForDecl Nothing Nothing mempty
