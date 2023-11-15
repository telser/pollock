{- |
Module: Pollock.Documentation.Metadata
Copyright: (c) Trevis Elser 2023
License: MIT

Maintainer: trevis@flipstone.com
Stability: experimental
Portability: portable
-}
module Pollock.Documentation.Metadata
  ( Metadata (..)
  , emptyMetadata
  , metaAppend
  , metadataHasSinceVersion
  ) where

import Control.Applicative ((<|>))

newtype Metadata = Metadata
  { version :: Maybe SinceVersion
  }

emptyMetadata :: Metadata
emptyMetadata =
  Metadata
    { version = Nothing
    }

-- | This is not a monoidal append, it uses '<|>' for the 'version'.
metaAppend :: Metadata -> Metadata -> Metadata
metaAppend (Metadata v1) (Metadata v2) = Metadata (v1 <|> v2)

metadataHasSinceVersion :: Metadata -> Bool
metadataHasSinceVersion (Metadata Nothing) = False
metadataHasSinceVersion (Metadata (Just vs)) = not $ null vs

type SinceVersion = [Int]
