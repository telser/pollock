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
  = DocAppend !Doc !Doc
  | DocString !String
  | DocParagraph !Doc
  | DocWarning !Doc
  | DocCodeBlock !Doc
  | DocProperty !String
  | DocExamples ![Example]

instance Semigroup Doc where
  a <> b = case (a, b) of
    (DocString s1, DocString s2) -> DocString (s1 <> s2)
    (DocAppend d (DocString s1), DocString s2) -> DocAppend d (DocString (s1 <> s2))
    (DocString s1, DocAppend (DocString s2) d) -> DocAppend (DocString (s1 <> s2)) d
    (d1, d2) -> DocAppend d1 d2

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
        DocString _ -> False
        DocProperty _ -> False
        DocCodeBlock d -> go d
        DocWarning d -> go d
        DocParagraph d -> go d
        DocAppend d1 d2 ->
          go d1 || go d2
   in
    go
