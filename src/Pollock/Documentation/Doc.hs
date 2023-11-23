{- |
Module:  Pollock.Documentation.Doc
Copyright: (c) Trevis Elser 2023
License:  MIT

Maintainer: trevis@flipstone.com
Stability: experimental
Portability: portable
-}
module Pollock.Documentation.Doc
  ( Doc (..)
  , docAppend
  , Example (..)
  , docHasWarning
  , docHasCodeBlock
  , docHasProperty
  , docHasExamples
  ) where

{- | A simplified model for haddock documentation. Note this diverges from haddock itself as many of
the complexities, particularly around display, are not needed for this use case.
-}
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

docHasWarning :: Doc -> Bool
docHasWarning =
  let
    go doc =
      case doc of
        DocWarning _ -> True
        DocEmpty -> False
        DocString _ -> False
        DocProperty _ -> False
        DocExamples _ -> False
        DocParagraph d -> go d
        DocCodeBlock d -> go d
        DocAppend d1 d2 ->
          go d1 || go d2
   in
    go

docHasCodeBlock :: Doc -> Bool
docHasCodeBlock =
  let
    go doc =
      case doc of
        DocCodeBlock _ -> True
        DocEmpty -> False
        DocString _ -> False
        DocProperty _ -> False
        DocExamples _ -> False
        DocWarning d -> go d
        DocParagraph d -> go d
        DocAppend d1 d2 ->
          go d1 || go d2
   in
    go

docHasProperty :: Doc -> Bool
docHasProperty =
  let
    go doc =
      case doc of
        DocProperty _ -> True
        DocEmpty -> False
        DocString _ -> False
        DocExamples _ -> False
        DocCodeBlock d -> go d
        DocWarning d -> go d
        DocParagraph d -> go d
        DocAppend d1 d2 ->
          go d1 || go d2
   in
    go

docHasExamples :: Doc -> Bool
docHasExamples =
  let
    go doc =
      case doc of
        DocExamples _ -> True
        DocEmpty -> False
        DocString _ -> False
        DocProperty _ -> False
        DocCodeBlock d -> go d
        DocWarning d -> go d
        DocParagraph d -> go d
        DocAppend d1 d2 ->
          go d1 || go d2
   in
    go
