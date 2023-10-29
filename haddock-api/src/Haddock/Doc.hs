module Haddock.Doc (docParagraph, docAppend,
                                  docConcat, metaDocConcat,
                                  metaDocAppend, emptyMetaDoc,
                                  metaAppend) where

import Control.Applicative ((<|>), empty)

import Haddock.Types
    ( DocH(DocParagraph, DocEmpty, DocString, DocAppend),
      MetaDoc(..),
      Meta(Meta) )

docConcat :: [DocH mod id] -> DocH mod id
docConcat = foldr docAppend DocEmpty

-- | Like 'docConcat' but also joins the 'Meta' info.
metaDocConcat :: [MetaDoc mod id] -> MetaDoc mod id
metaDocConcat = foldr metaDocAppend emptyMetaDoc

-- | We do something perhaps unexpected here and join the meta info
-- in ‘reverse’: this results in the metadata from the ‘latest’
-- paragraphs taking precedence.
metaDocAppend :: MetaDoc mod id -> MetaDoc mod id -> MetaDoc mod id
metaDocAppend (MetaDoc { _meta = m, _doc = d })
              (MetaDoc { _meta = m', _doc = d' }) =
  MetaDoc { _meta = m' `metaAppend` m, _doc = d `docAppend` d' }

-- | This is not a monoidal append, it uses '<|>' for the '_version' and
-- '_package'.
metaAppend :: Meta -> Meta -> Meta
metaAppend (Meta v1 p1) (Meta v2 p2) = Meta (v1 <|> v2) (p1 <|> p2)

emptyMetaDoc :: MetaDoc mod id
emptyMetaDoc = MetaDoc { _meta = emptyMeta, _doc = DocEmpty }

emptyMeta :: Meta
emptyMeta = Meta empty empty

docAppend :: DocH mod id -> DocH mod id -> DocH mod id
docAppend DocEmpty d = d
docAppend d DocEmpty = d
docAppend (DocString s1) (DocString s2) = DocString (s1 ++ s2)
docAppend (DocAppend d (DocString s1)) (DocString s2) = DocAppend d (DocString (s1 ++ s2))
docAppend (DocString s1) (DocAppend (DocString s2) d) = DocAppend (DocString (s1 ++ s2)) d
docAppend d1 d2 = DocAppend d1 d2

-- again to make parsing easier - we spot a paragraph whose only item
-- is a DocMonospaced and make it into a DocCodeBlock
docParagraph :: DocH mod id -> DocH mod id
docParagraph p = DocParagraph p
