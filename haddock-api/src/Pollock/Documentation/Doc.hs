{- |
Module:  Pollock.Documentation.Doc
Copyright: (c) Trevis Elser 2023
License:  MIT

Maintainer: trevis@flipstone.com
Stability: experimental
Portability: portable

-}

module Pollock.Documentation.Doc
  (Doc(..), docAppend, Example(..) ) where

-- | A simplified model for haddock documentation. Note this diverges from haddock itself as many of
-- the complexities, particularly around display, are not needed for this use case.
data Doc
  = DocEmpty
  | DocAppend !Doc !Doc
  | DocString !String
  | DocParagraph !Doc
  | DocWarning !Doc
  | DocCodeBlock !Doc
  | DocProperty !String
  | DocExamples ![Example]

docAppend :: Doc -> Doc -> Doc
docAppend DocEmpty d = d
docAppend d DocEmpty = d
docAppend (DocString s1) (DocString s2) = DocString (s1 <> s2)
docAppend (DocAppend d (DocString s1)) (DocString s2) = DocAppend d (DocString (s1 <> s2))
docAppend (DocString s1) (DocAppend (DocString s2) d) = DocAppend (DocString (s1 <> s2)) d
docAppend d1 d2 = DocAppend d1 d2

data Example = Example
  { exampleExpression :: !String
  , exampleResult :: ![String]
  }
