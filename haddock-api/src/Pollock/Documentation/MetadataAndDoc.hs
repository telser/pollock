{- |
Module      : Pollock.Documentation.MetadataAndDoc
Copyright   : (c) Trevis Elser 2023
License     :  MIT
Maintainer: trevis@flipstone.com
Stability: experimental
Portability: non-portable
-}
module Pollock.Documentation.MetadataAndDoc
  ( MetaAndDoc (..)
  , withEmptyMetadata
  , metaAndDocConcat
  , metaAndDocAppend
  ) where

import Pollock.Documentation.Doc ( docAppend, Doc(DocEmpty) )
import Pollock.Documentation.Metadata
    ( Metadata, emptyMetadata, metaAppend )

data MetaAndDoc = MetaAndDoc
  { meta :: !Metadata
  , doc :: !Doc
  }

withEmptyMetadata :: Doc -> MetaAndDoc
withEmptyMetadata d =
  MetaAndDoc
    { meta = emptyMetadata
    , doc = d
    }

metaAndDocConcat :: [MetaAndDoc] -> MetaAndDoc
metaAndDocConcat = foldr metaAndDocAppend emptyMetaAndDoc

-- Append where for metadata prefence is on the right, but for doc it is on the left.
metaAndDocAppend :: MetaAndDoc -> MetaAndDoc -> MetaAndDoc
metaAndDocAppend md1 md2 =
  MetaAndDoc
    { meta = metaAppend (meta md2) (meta md1)
    , doc = docAppend (doc md1) (doc md2)
    }

emptyMetaAndDoc :: MetaAndDoc
emptyMetaAndDoc = MetaAndDoc{meta = emptyMetadata, doc = DocEmpty}
