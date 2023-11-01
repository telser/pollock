module Haddock.Doc
  ( metaDocConcat
  , metaDocAppend
  ) where

import Control.Applicative ((<|>))

import Haddock.Types
  ( Doc (DocAppend, DocEmpty, DocString)
  , Meta (Meta)
  , MetaDoc (..)
  , emptyMetadata
  )

-- | Like 'docConcat' but also joins the 'Meta' info.
metaDocConcat :: [MetaDoc] -> MetaDoc
metaDocConcat = foldr metaDocAppend emptyMetaDoc

{- | We do something perhaps unexpected here and join the meta info
in ‘reverse’: this results in the metadata from the ‘latest’
paragraphs taking precedence.
-}
metaDocAppend :: MetaDoc -> MetaDoc -> MetaDoc
metaDocAppend
  (MetaDoc{meta = m, doc = d})
  (MetaDoc{meta = m', doc = d'}) =
    MetaDoc{meta = m' `metaAppend` m, doc = d `docAppend` d'}

-- | This is not a monoidal append, it uses '<|>' for the 'version'.
metaAppend :: Meta -> Meta -> Meta
metaAppend (Meta v1) (Meta v2) = Meta (v1 <|> v2)

emptyMetaDoc :: MetaDoc
emptyMetaDoc = MetaDoc{meta = emptyMetadata, doc = DocEmpty}

docAppend :: Doc -> Doc -> Doc
docAppend DocEmpty d = d
docAppend d DocEmpty = d
docAppend (DocString s1) (DocString s2) = DocString (s1 ++ s2)
docAppend (DocAppend d (DocString s1)) (DocString s2) = DocAppend d (DocString (s1 ++ s2))
docAppend (DocString s1) (DocAppend (DocString s2) d) = DocAppend (DocString (s1 ++ s2)) d
docAppend d1 d2 = DocAppend d1 d2
