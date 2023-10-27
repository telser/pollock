-- | Bridge impedance mismatch of different @base@ versions back till @base-4.5@ (GHC 7.4.2)
module Haddock.CompatPrelude
    ( ($>)
    , isSymbolChar
    ) where

import           Data.Functor                ( ($>) )

import           Text.Read.Lex                      (isSymbolChar)
