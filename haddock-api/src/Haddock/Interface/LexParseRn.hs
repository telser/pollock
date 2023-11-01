{-# LANGUAGE FlexibleContexts #-}

{- |
Module      :  Haddock.Interface.LexParseRn
Copyright   :  (c) Isaac Dupree 2009,
                   Mateusz Kowalczyk 2013
License     :  BSD-like

Maintainer  :  haddock@projects.haskell.org
Stability   :  experimental
Portability :  portable
-}
module Haddock.Interface.LexParseRn
  ( processDocStringParas
  , processDocStrings
  , processModuleHeader
  ) where

import Data.List ((\\))

import qualified Pollock.CompatGHC as CompatGHC

import Haddock.Doc (metaDocConcat)
import Haddock.Interface.ParseModuleHeader (parseModuleHeader)
import qualified Haddock.Parser.Monad as PM
import Haddock.Types

processDocStrings ::
  [CompatGHC.HsDocString]
  -> Maybe MDoc
processDocStrings strs =
  let mdoc = metaDocConcat $ fmap processDocStringParas strs
   in case mdoc of
        -- We check that we don't have any version info to render instead
        -- of just checking if there is no comment: there may not be a
        -- comment but we still want to pass through any meta data.
        MetaDoc{meta = Meta Nothing, doc = DocEmpty} -> Nothing
        x -> (Just x)

processDocStringParas ::
  CompatGHC.HsDocString -> MDoc
processDocStringParas =
  PM.parseParas' . CompatGHC.renderHsDocString

processModuleHeader ::
  CompatGHC.DynFlags
  -> CompatGHC.SafeHaskellMode
  -> Maybe CompatGHC.HsDocString
  -> HaddockModInfo
processModuleHeader dflags safety mayStr =
  let flags :: [CompatGHC.Extension]
      -- We remove the flags implied by the language setting and we display the language instead
      flags =
        CompatGHC.enumSetToList (CompatGHC.extensionFlags dflags)
          \\ CompatGHC.languageExtensions (CompatGHC.language dflags)
      -- TODO-POL We probably shouldn't overwrite the extensions, language, or saftey with the inferred and instead report exactly as it is listed in the code
      hmi =
        case mayStr of
          Nothing -> emptyHaddockModInfo
          Just hds ->
            parseModuleHeader $ CompatGHC.renderHsDocString hds
   in hmi
        { hmi_safety = Just $ CompatGHC.showPpr dflags safety
        , hmi_language = CompatGHC.language dflags
        , hmi_extensions = flags
        }
