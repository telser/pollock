{- |
Module:  Pollock.ModuleInfo.ModuleHeader
Copyright: (c) Trevis Elser 2023
License:  MIT
Maintainer: trevis@flipstone.com
Stability: experimental
Portability: not-portable
-}
module Pollock.ModuleInfo.ModuleHeader
  ( ModuleHeader (..)
  , processModuleHeader
  ) where

import qualified Control.Applicative as App
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe

import qualified Pollock.CompatGHC as CompatGHC

-- FIXME Consider safety, language and extensions if they are manually present? Does anyone
-- actually do that? Unclear, but could be useful.
data ModuleHeader = ModuleHeader
  { description :: !(Maybe String)
  -- ^ The description field of the Haddock module header.
  , copyright :: !(Maybe String)
  -- ^ The copyright field of the Haddock module header.
  , license :: !(Maybe String)
  -- ^ The license field of the Haddock module header.
  , maintainer :: !(Maybe String)
  -- ^ The maintainer field of the Haddock module header.
  , stability :: !(Maybe String)
  -- ^ The stability field of the Haddock module header.
  , portability :: !(Maybe String)
  -- ^ The portability field of the Haddock module header.
  }

emptyHaddockModInfo :: ModuleHeader
emptyHaddockModInfo =
  ModuleHeader
    { description = Nothing
    , copyright = Nothing
    , license = Nothing
    , maintainer = Nothing
    , stability = Nothing
    , portability = Nothing
    }

processModuleHeader ::
  Maybe CompatGHC.HsDocString
  -> ModuleHeader
processModuleHeader mayStr =
  case mayStr of
    Nothing -> emptyHaddockModInfo
    Just hds ->
      parseModuleHeader $ CompatGHC.renderHsDocString hds

parseModuleHeader ::
  String -> ModuleHeader
parseModuleHeader str0 =
  let
    kvs :: [(String, String)]
    kvs = Maybe.fromMaybe mempty $ runP fields str0

    -- trim whitespaces
    trim :: String -> String
    trim = dropWhile Char.isSpace . reverse . dropWhile Char.isSpace . reverse

    getKey :: String -> Maybe String
    getKey key = fmap trim (lookup key kvs)

    descriptionOpt = getKey "Description"
    copyrightOpt = getKey "Copyright"
    licenseOpt = getKey "License"
    licenceOpt = getKey "Licence"
    spdxLicenceOpt = getKey "SPDX-License-Identifier"
    maintainerOpt = getKey "Maintainer"
    stabilityOpt = getKey "Stability"
    portabilityOpt = getKey "Portability"
   in
    ModuleHeader
      { description = descriptionOpt
      , copyright = copyrightOpt
      , license = spdxLicenceOpt App.<|> licenseOpt App.<|> licenceOpt
      , maintainer = maintainerOpt
      , stability = stabilityOpt
      , portability = portabilityOpt
      }

-------------------------------------------------------------------------------
-- Small parser to parse module header.
-------------------------------------------------------------------------------

{- | The below is a small parser framework how we read keys.

all fields in the header are optional and have the form

[spaces1][field name][spaces] ":"
   [text]"\n" ([spaces2][space][text]"\n" | [spaces]"\n")*
where each [spaces2] should have [spaces1] as a prefix.

Thus for the key "Description",

> Description : this is a
>    rather long
>
>    description
>
> The module comment starts here

the value will be "this is a .. description" and the rest will begin
at "The module comment".
-}

{- | 'C' is a 'Char' carrying its column.

This let us make an indentation-aware parser, as we know current indentation.
by looking at the next character in the stream ('curInd').

Thus we can munch all spaces but only not-spaces which are indented.
-}
data C = C {-# UNPACK #-} !Int Char

newtype P a = P {unP :: [C] -> Maybe ([C], a)}

instance Functor P where
  fmap f (P pn) =
    P ((fmap . fmap) f . pn)

instance Applicative P where
  pure x = P $ \s -> Just (s, x)
  (<*>) m1 m2 =
    P $ \t0 ->
      case unP m1 t0 of
        Nothing -> Nothing
        Just (t1, z) ->
          (fmap . fmap) z (unP m2 t1)

instance Monad P where
  m >>= k =
    P $ \s0 ->
      case unP m s0 of
        Nothing -> Nothing
        Just (s1, x) -> unP (k x) s1
  return = pure

instance App.Alternative P where
  empty = P $ const Nothing
  a <|> b = P $ \s -> unP a s App.<|> unP b s

runP :: P a -> String -> Maybe a
runP p input = fmap snd (unP p input')
 where
  input' =
    concat
      [ zipWith C [0 ..] l <> [C (length l) '\n']
      | l <- lines input
      ]

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

curInd :: P Int
curInd = P $ \s -> Just . (,) s $ case s of
  [] -> 0
  C i _ : _ -> i

munch :: (Int -> Char -> Bool) -> P String
munch p = P $ \cs ->
  let (xs, ys) = takeWhileMaybe p' cs in Just (ys, xs)
 where
  p' (C i c)
    | p i c = Just c
    | otherwise = Nothing

munch1 :: (Int -> Char -> Bool) -> P String
munch1 p = P $ \s -> case s of
  [] -> Nothing
  (c : cs)
    | Just c' <- p' c -> let (xs, ys) = takeWhileMaybe p' cs in Just (ys, c' : xs)
    | otherwise -> Nothing
 where
  p' (C i c)
    | p i c = Just c
    | otherwise = Nothing

char :: Char -> P Char
char c = P $ \s -> case s of
  [] -> Nothing
  (C _ c' : cs)
    | c == c' -> Just (cs, c)
    | otherwise -> Nothing

skipSpaces :: P ()
skipSpaces = P $ \cs -> Just (dropWhile (\(C _ c) -> Char.isSpace c) cs, ())

takeWhileMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
takeWhileMaybe f = go
 where
  go xs0@[] = ([], xs0)
  go xs0@(x : xs) = case f x of
    Just y -> let (ys, zs) = go xs in (y : ys, zs)
    Nothing -> ([], xs0)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

field :: Int -> P (String, String)
field i = do
  fn <- munch1 $ \_ c -> Char.isAlpha c || c == '-'
  skipSpaces
  _ <- char ':'
  skipSpaces
  val <- munch $ \j c -> Char.isSpace c || j > i
  pure (fn, val)

fields :: P [(String, String)]
fields = do
  skipSpaces
  i <- curInd
  App.many (field i)
