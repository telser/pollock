{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Haddock.Parser
Copyright   :  (c) Mateusz Kowalczyk 2013,
                   Simon Hengel      2013
License     :  BSD-like

Maintainer  :  haddock@projects.haskell.org
Stability   :  experimental
Portability :  portable
-}
module Haddock.Parser
  ( parseParas
  , parseString
  ) where

import Control.Applicative
import Control.Arrow (first)
import qualified Control.Monad as M
import Data.Char (chr, isSpace)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec (try)
import qualified Text.Parsec as Parsec
import Prelude hiding (takeWhile)

import GHC.Data.FastString (fsLit)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Session (DynFlags)
import GHC.Parser (parseIdentifier)
import GHC.Parser.Lexer (ParseResult (PFailed, POk), initParserState, unP)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.SrcLoc (GenLocated (..), mkRealSrcLoc)

import Haddock.Doc
import Haddock.Parser.Identifier
import Haddock.Parser.Monad
import Haddock.Parser.Util
import Haddock.Types

parseParas :: DynFlags -> Maybe Package -> String -> MetaDoc mod (Wrap NsRdrName)
parseParas d p = overDoc (overIdentifier (parseIdent d)) . parseParas' p

parseString :: DynFlags -> String -> DocH mod (Wrap NsRdrName)
parseString d = overIdentifier (parseIdent d) . parseString'

parseIdent :: DynFlags -> Namespace -> String -> Maybe (Wrap NsRdrName)
parseIdent dflags ns str0 =
  case unP parseIdentifier (pstate str1) of
    POk _ (L _ name)
      -- Guards against things like 'Q.--', 'Q.case', etc.
      -- See https://github.com/haskell/haddock/issues/952 and Trac #14109
      | Qual _ occ <- name
      , PFailed{} <- unP parseIdentifier (pstate (occNameString occ)) ->
          Nothing
      | otherwise ->
          Just (wrap (NsRdrName ns name))
    PFailed{} -> Nothing
 where
  realSrcLc = mkRealSrcLoc (fsLit "<unknown file>") 0 0
  pstate str = initParserState (initParserOpts dflags) (stringToStringBuffer str) realSrcLc
  (wrap, str1) = case str0 of
    '(' : s@(c : _)
      | c /= ','
      , c /= ')' -> -- rule out tuple names
          (Parenthesized, init s)
    '`' : s@(_ : _) -> (Backticked, init s)
    _ -> (Unadorned, str0)

{- | Maps over 'DocIdentifier's over 'String' with potentially failing
conversion using user-supplied function. If the conversion fails,
the identifier is deemed to not be valid and is treated as a
regular string.
-}
overIdentifier ::
  (Namespace -> String -> Maybe a)
  -> DocH mod Identifier
  -> DocH mod a
overIdentifier f d = g d
 where
  g (DocIdentifier (Identifier ns o x e)) = case f ns x of
    Nothing -> DocString $ renderNs ns ++ [o] ++ x ++ [e]
    Just x' -> DocIdentifier x'
  g DocEmpty = DocEmpty
  g (DocAppend x x') = DocAppend (g x) (g x')
  g (DocString x) = DocString x
  g (DocParagraph x) = DocParagraph $ g x
  g (DocIdentifierUnchecked x) = DocIdentifierUnchecked x
  g (DocWarning x) = DocWarning $ g x
  g (DocCodeBlock x) = DocCodeBlock $ g x
  g (DocProperty x) = DocProperty x
  g (DocExamples x) = DocExamples x

choice' :: [Parser a] -> Parser a
choice' [] = empty
choice' [p] = p
choice' (p : ps) = try p <|> choice' ps

parse :: Parser a -> Text -> (ParserState, a)
parse p = either err id . parseOnly (p <* Parsec.eof)
 where
  err = error . ("Haddock.Parser.parse: " ++)

{- | Main entry point to the parser. Appends the newline character
to the input string.
-}
parseParas' ::
  Maybe Package
  -> String
  -- ^ String to parse
  -> MetaDoc mod Identifier
parseParas' pkg input = case parseParasState input of
  (state, a) ->
    MetaDoc
      { _meta =
          Meta
            { _version = parserStateSince state
            , _package = pkg
            }
      , _doc = a
      }

parseParasState :: String -> (ParserState, DocH mod Identifier)
parseParasState = parse (emptyLines *> p) . T.pack . (++ "\n") . filter (/= '\r')
 where
  p :: Parser (DocH mod Identifier)
  p = docConcat <$> many (paragraph <* emptyLines)

  emptyLines :: Parser ()
  emptyLines = M.void $ many (try (skipHorizontalSpace *> "\n"))

-- | Variant of 'parseText' for 'String' instead of 'Text'
parseString' :: String -> DocH mod Identifier
parseString' = parseText . T.pack

{- | Parse a text paragraph. Actually just a wrapper over 'parseParagraph' which
drops leading whitespace.
-}
parseText :: Text -> DocH mod Identifier
parseText = parseParagraph . T.dropWhile isSpace . T.filter (/= '\r')

parseParagraph :: Text -> DocH mod Identifier
parseParagraph = snd . parse p
 where
  p :: Parser (DocH mod Identifier)
  p =
    docConcat
      <$> many
        ( choice'
            [ -- monospace
              -- ,
              -- anchor
              -- ,
              identifier
            , -- , moduleName
              -- , picture
              -- , mathDisplay
              -- , mathInline
              -- , markdownImage
              -- , markdownLink
              -- , hyperlink
              -- , bold
              -- , emphasis
              encodedChar
            , string'
            , skipSpecialChar
            ]
        )

{- | Parses and processes
<https://en.wikipedia.org/wiki/Numeric_character_reference Numeric character references>

>>> parseString "&#65;"
DocString "A"
-}
encodedChar :: Parser (DocH mod a)
encodedChar = "&#" *> c <* ";"
 where
  c = DocString . M.return . chr <$> num
  num = hex <|> decimal
  hex = ("x" <|> "X") *> hexadecimal

{- | List of characters that we use to delimit any special markup.
Once we have checked for any of these and tried to parse the
relevant markup, we can assume they are used as regular text.
-}
specialChar :: [Char]
specialChar = "_/<@\"&'`#[ "

{- | Plain, regular parser for text. Called as one of the last parsers
to ensure that we have already given a chance to more meaningful parsers
before capturing their characters.
-}
string' :: Parser (DocH mod a)
string' = DocString . unescape . T.unpack <$> takeWhile1_ (`notElem` specialChar)
 where
  unescape "" = ""
  unescape ('\\' : x : xs) = x : unescape xs
  unescape (x : xs) = x : unescape xs

{- | Skips a single special character and treats it as a plain string.
This is done to skip over any special characters belonging to other
elements but which were not deemed meaningful at their positions.
-}
skipSpecialChar :: Parser (DocH mod a)
skipSpecialChar = DocString . M.return <$> Parsec.oneOf specialChar

-- | Like `takeWhile`, but unconditionally take escaped characters.
takeWhile_ :: (Char -> Bool) -> Parser Text
takeWhile_ p = scan p_ False
 where
  p_ escaped c
    | escaped = Just False
    | not $ p c = Nothing
    | otherwise = Just (c == '\\')

-- | Like 'takeWhile1', but unconditionally take escaped characters.
takeWhile1_ :: (Char -> Bool) -> Parser Text
takeWhile1_ = M.mfilter (not . T.null) . takeWhile_

-- | Paragraph parser, called by 'parseParas'.
paragraph :: Parser (DocH mod Identifier)
paragraph =
  choice'
    [ examples
    , do
        choice'
          [ since
          , birdtracks
          , codeblock
          , property
          , docParagraph <$> textParagraph
          ]
    ]

-- | Parse \@since annotations.
since :: Parser (DocH mod a)
since = ("@since " *> version <* skipHorizontalSpace <* endOfLine) M.>>= setSince M.>> M.return DocEmpty
 where
  version = decimal `Parsec.sepBy1` "."

textParagraph :: Parser (DocH mod Identifier)
textParagraph = parseText . T.intercalate "\n" <$> some nonEmptyLine

{- | Blocks of text of the form:

>> foo
>> bar
>> baz
-}
birdtracks :: Parser (DocH mod a)
birdtracks = DocCodeBlock . DocString . T.unpack . T.intercalate "\n" . stripSpace <$> some line
 where
  line = try (skipHorizontalSpace *> ">" *> takeLine)

stripSpace :: [Text] -> [Text]
stripSpace = fromMaybe <*> M.mapM strip'
 where
  strip' t = case T.uncons t of
    Nothing -> Just ""
    Just (' ', t') -> Just t'
    _ -> Nothing

{- | Parses examples. Examples are a paragraph level entity (separated by an empty line).
Consecutive examples are accepted.
-}
examples :: Parser (DocH mod a)
examples = DocExamples <$> (many (try (skipHorizontalSpace *> "\n")) *> go)
 where
  go :: Parser [Example]
  go = do
    prefix <- takeHorizontalSpace <* ">>>"
    expr <- takeLine
    (rs, es) <- resultAndMoreExamples
    M.return (makeExample prefix expr rs : es)
   where
    moreExamples :: Parser ([Text], [Example])
    moreExamples = (,) [] <$> go

    result :: Parser ([Text], [Example])
    result = first . (:) <$> nonEmptyLine <*> resultAndMoreExamples

    resultAndMoreExamples :: Parser ([Text], [Example])
    resultAndMoreExamples = choice' [moreExamples, result, pure ([], [])]

  makeExample :: Text -> Text -> [Text] -> Example
  makeExample prefix expression res =
    Example (T.unpack (T.strip expression)) result
   where
    result = map (T.unpack . substituteBlankLine . tryStripPrefix) res

    tryStripPrefix xs = fromMaybe xs (T.stripPrefix prefix xs)

    substituteBlankLine "<BLANKLINE>" = ""
    substituteBlankLine xs = xs

nonEmptyLine :: Parser Text
nonEmptyLine = try (M.mfilter (T.any (not . isSpace)) takeLine)

takeLine :: Parser Text
takeLine = try (takeWhile (/= '\n') <* endOfLine)

endOfLine :: Parser ()
endOfLine = M.void "\n" <|> Parsec.eof

{- | Property parser.

>>> snd <$> parseOnly property "prop> hello world"
Right (DocProperty "hello world")
-}
property :: Parser (DocH mod a)
property = DocProperty . T.unpack . T.strip <$> ("prop>" *> takeWhile1 (/= '\n'))

{- |
Paragraph level codeblock. Anything between the two delimiting \@ is parsed
for markup.
-}
codeblock :: Parser (DocH mod Identifier)
codeblock =
  DocCodeBlock . parseParagraph . dropSpaces
    <$> ("@" *> skipHorizontalSpace *> "\n" *> block' <* "@")
 where
  dropSpaces xs =
    case splitByNl xs of
      [] -> xs
      ys -> case T.uncons (last ys) of
        Just (' ', _) -> case M.mapM dropSpace ys of
          Nothing -> xs
          Just zs -> T.intercalate "\n" zs
        _ -> xs

  -- This is necessary because ‘lines’ swallows up a trailing newline
  -- and we lose information about whether the last line belongs to @ or to
  -- text which we need to decide whether we actually want to be dropping
  -- anything at all.
  splitByNl =
    unfoldr
      ( \x -> case T.uncons x of
          Just ('\n', x') -> Just (T.span (/= '\n') x')
          _ -> Nothing
      )
      . ("\n" <>)

  dropSpace t = case T.uncons t of
    Nothing -> Just ""
    Just (' ', t') -> Just t'
    _ -> Nothing

  block' = scan p False
   where
    p isNewline c
      | isNewline && c == '@' = Nothing
      | isNewline && isSpace c = Just isNewline
      | otherwise = Just $ c == '\n'

-- | Parses identifiers with help of 'parseValid'.
identifier :: Parser (DocH mod Identifier)
identifier = DocIdentifier <$> parseValid
