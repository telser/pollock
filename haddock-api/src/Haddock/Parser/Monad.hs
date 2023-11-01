{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Documentation.Haddock.Parser.Monad
Copyright   :  (c) Alec Theriault 2018-2019,
License     :  BSD-like

Maintainer  :  haddock@projects.haskell.org
Stability   :  experimental
Portability :  portable

Defines the Parsec monad over which all parsing is done and also provides
more efficient versions of the usual parsec combinator functions (but
specialized to 'T.Text').
-}
module Haddock.Parser.Monad where

import qualified Control.Applicative as App
import qualified Control.Monad as M
import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Char as Char
import qualified Data.Text as T

import Haddock.Doc
import Haddock.Types
import Prelude hiding (takeWhile)

since :: AttoText.Parser MetaDoc
since = do
  skipHorizontalSpace
  _ <- AttoText.string "@since "
  s <- AttoText.sepBy1 AttoText.decimal "."
  skipHorizontalSpace
  let
    metadata =
      Meta
        { version = Just s
        }
  App.pure $ MetaDoc metadata DocEmpty

skipHorizontalSpace :: AttoText.Parser ()
skipHorizontalSpace =
  AttoText.skipWhile AttoText.isHorizontalSpace

takeLine :: AttoText.Parser T.Text
takeLine = takeToEndOfLine

takeNonEmptyLine :: AttoText.Parser T.Text
takeNonEmptyLine =
  M.mfilter (T.any (not . Char.isSpace)) takeLine

birdtracks :: AttoText.Parser MetaDoc
birdtracks =
  let line = do
        skipHorizontalSpace
        _ <- AttoText.string ">"
        takeLine
   in fmap (withEmptyMetadata . DocCodeBlock . docStringFromText . T.intercalate "\n") $
        AttoText.many1 line

paragraph :: AttoText.Parser MetaDoc
paragraph =
  AttoText.choice
    [ since
    , birdtracks
    , fmap withEmptyMetadata codeblock
    , fmap withEmptyMetadata property
    , fmap (withEmptyMetadata . docStringFromText) takeLine
    , fmap (withEmptyMetadata . DocParagraph) textParagraph
    ]

parseText :: T.Text -> Doc
parseText =
  either error id
    . AttoText.parseOnly (fmap docStringFromText AttoText.takeText)
    . (T.filter (/= '\r'))

docStringFromText :: T.Text -> Doc
docStringFromText = DocString . T.unpack

textParagraph :: AttoText.Parser Doc
textParagraph = do
  lines' <- AttoText.many1 takeNonEmptyLine
  App.pure $ (docStringFromText . T.intercalate "\n") lines'

parseParas' :: String -> MetaDoc
parseParas' = either error id . AttoText.parseOnly parseParas . T.pack . filter (/= '\r')

parseParas :: AttoText.Parser MetaDoc
parseParas =
  fmap metaDocConcat . AttoText.many' $ do
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
