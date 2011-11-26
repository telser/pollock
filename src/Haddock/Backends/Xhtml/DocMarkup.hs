-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html.DocMarkup
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml.DocMarkup (
  docToHtml,
  rdrDocToHtml,
  origDocToHtml,

  docElement, docSection, maybeDocSection,
) where


import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Utils
import Haddock.GhcUtils
import Haddock.Types
import Haddock.Utils

import Text.XHtml hiding ( name, title, p, quote )

import GHC


parHtmlMarkup :: (a -> Html) -> DocMarkup a Html
parHtmlMarkup ppId = Markup {
  markupEmpty         = noHtml,
  markupString        = toHtml,
  markupParagraph     = paragraph,
  markupAppend        = (+++),
  markupIdentifier    = thecode . ppId,
  markupModule        = \m -> let (mdl,ref) = break (=='#') m
                              in ppModuleRef (mkModuleNoPackage mdl) ref,
  markupEmphasis      = emphasize,
  markupMonospaced    = thecode,
  markupUnorderedList = unordList,
  markupOrderedList   = ordList,
  markupDefList       = defList,
  markupCodeBlock     = pre,
  markupURL           = \url -> anchor ! [href url] << url,
  markupAName         = \aname -> namedAnchor aname << "",
  markupPic           = \path -> image ! [src path],
  markupExample       = examplesToHtml
  }
  where
    examplesToHtml l = pre (concatHtml $ map exampleToHtml l) ! [theclass "screen"]

    exampleToHtml (Example expression result) = htmlExample
      where
        htmlExample = htmlPrompt +++ htmlExpression +++ toHtml (unlines result)
        htmlPrompt = (thecode . toHtml $ ">>> ") ! [theclass "prompt"]
        htmlExpression = (strong . thecode . toHtml $ expression ++ "\n") ! [theclass "userinput"]


-- If the doc is a single paragraph, don't surround it with <P> (this causes
-- ugly extra whitespace with some browsers).  FIXME: Does this still apply?
docToHtml :: Qualification -> Doc DocName -> Html
docToHtml qual = markup fmt . cleanup
  where fmt = parHtmlMarkup (ppDocName qual)


origDocToHtml :: Doc Name -> Html
origDocToHtml = markup fmt . cleanup
  where fmt = parHtmlMarkup ppName


rdrDocToHtml :: Doc RdrName -> Html
rdrDocToHtml = markup fmt . cleanup
  where fmt = parHtmlMarkup ppRdrName


docElement :: (Html -> Html) -> Html -> Html
docElement el content_ =
  if isNoHtml content_
    then el ! [theclass "doc empty"] << spaceHtml
    else el ! [theclass "doc"] << content_


docSection :: Qualification -> Doc DocName -> Html
docSection qual = (docElement thediv <<) . docToHtml qual


maybeDocSection :: Qualification -> Maybe (Doc DocName) -> Html
maybeDocSection qual = maybe noHtml (docSection qual)


cleanup :: Doc a -> Doc a
cleanup = markup fmtUnParagraphLists
  where
    -- If there is a single paragraph, then surrounding it with <P>..</P>
    -- can add too much whitespace in some browsers (eg. IE).  However if
    -- we have multiple paragraphs, then we want the extra whitespace to
    -- separate them.  So we catch the single paragraph case and transform it
    -- here. We don't do this in code blocks as it eliminates line breaks.
    unParagraph :: Doc a -> Doc a
    unParagraph (DocParagraph d) = d
    unParagraph doc              = doc

    fmtUnParagraphLists :: DocMarkup a (Doc a)
    fmtUnParagraphLists = idMarkup {
      markupUnorderedList = DocUnorderedList . map unParagraph,
      markupOrderedList   = DocOrderedList   . map unParagraph
      }
