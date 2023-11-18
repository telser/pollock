{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Pollock.Documentation.Parser
Copyright: (c) Trevis Elser 2023
License: MIT
Maintainer: trevis@flipstone.com
Stability: experimental
Portability: non-portable
-}
module Pollock.Documentation.Parser
  ( processDocStringParas
  , processDocStrings
  , parseText
  )
where

import qualified Control.Applicative as App
import qualified Control.Monad as M
import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Char as Char
import qualified Data.Text as T

import qualified Pollock.CompatGHC as CompatGHC
import Pollock.Documentation.Doc
  ( Doc
      ( DocCodeBlock
      , DocEmpty
      , DocParagraph
      , DocProperty
      , DocString
      )
  )
import Pollock.Documentation.Metadata (Metadata (Metadata, version))
import Pollock.Documentation.MetadataAndDoc (MetaAndDoc (..), metaAndDocConcat, withEmptyMetadata)

parseText :: T.Text -> Doc
parseText =
  either error id
    . AttoText.parseOnly (fmap docStringFromText AttoText.takeText)
    . T.filter (/= '\r')

processDocStringParas ::
  CompatGHC.HsDocString -> MetaAndDoc
processDocStringParas =
  either error id
    . AttoText.parseOnly parseParas
    . T.pack
    . filter (/= '\r')
    . CompatGHC.renderHsDocString

processDocStrings ::
  [CompatGHC.HsDocString]
  -> Maybe MetaAndDoc
processDocStrings strs =
  case metaAndDocConcat $ fmap processDocStringParas strs of
    -- We check that we don't have any version info to render instead
    -- of just checking if there is no comment: there may not be a
    -- comment but we still want to pass through any meta data.
    MetaAndDoc{meta = Metadata Nothing, doc = DocEmpty} -> Nothing
    x -> Just x

since :: AttoText.Parser MetaAndDoc
since = do
  skipHorizontalSpace
  _ <- AttoText.string "@since "
  s <- AttoText.sepBy1 AttoText.decimal "."
  skipHorizontalSpace
  let
    metadata =
      Metadata
        { version = Just s
        }
  pure $ MetaAndDoc metadata DocEmpty

skipHorizontalSpace :: AttoText.Parser ()
skipHorizontalSpace =
  AttoText.skipWhile AttoText.isHorizontalSpace

takeLine :: AttoText.Parser T.Text
takeLine = takeToEndOfLine

takeNonEmptyLine :: AttoText.Parser T.Text
takeNonEmptyLine =
  M.mfilter (T.any (not . Char.isSpace)) takeLine

birdtracks :: AttoText.Parser MetaAndDoc
birdtracks =
  let line = do
        skipHorizontalSpace
        _ <- AttoText.string ">"
        takeLine
   in fmap (withEmptyMetadata . DocCodeBlock . docStringFromText . T.intercalate "\n") $
        AttoText.many1 line

paragraph :: AttoText.Parser MetaAndDoc
paragraph =
  AttoText.choice
    [ since
    , birdtracks
    , fmap withEmptyMetadata codeblock
    , fmap withEmptyMetadata property
    , fmap (withEmptyMetadata . docStringFromText) takeLine
    , fmap (withEmptyMetadata . DocParagraph) textParagraph
    ]

docStringFromText :: T.Text -> Doc
docStringFromText = DocString . T.unpack

textParagraph :: AttoText.Parser Doc
textParagraph = do
  lines' <- AttoText.many1 takeNonEmptyLine
  App.pure $ (docStringFromText . T.intercalate "\n") lines'

parseParas :: AttoText.Parser MetaAndDoc
parseParas =
  fmap metaAndDocConcat . AttoText.many' $ do
    p <- paragraph
    consumeEmptyLines
    App.pure p

{- | Property parser.

>>> snd <$> parseOnly property "prop> hello world"
Right (DocProperty "hello world")
-}
property :: AttoText.Parser Doc
property =
  fmap (DocProperty . T.unpack . T.strip) $ do
    _ <- AttoText.string "prop>"
    takeToEndOfLine

{- |
Paragraph level codeblock. Anything between the two delimiting \@ is parsed
for markup.
-}
codeblock :: AttoText.Parser Doc
codeblock = do
  _ <- AttoText.string "@"
  skipHorizontalSpace
  AttoText.endOfLine
  blockDoc <- textParagraph
  _ <- AttoText.string "@"
  App.pure $ DocCodeBlock blockDoc

takeToEndOfLine :: AttoText.Parser T.Text
takeToEndOfLine = AttoText.takeWhile1 (not . AttoText.isEndOfLine)

consumeEmptyLines :: AttoText.Parser ()
consumeEmptyLines =
  M.void . AttoText.many' $ do
    skipHorizontalSpace
    AttoText.endOfLine
