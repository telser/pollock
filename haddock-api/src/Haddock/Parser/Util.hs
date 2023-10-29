{- |
Module      :  Documentation.Haddock.Parser.Util
Copyright   :  (c) Mateusz Kowalczyk 2013-2014,
                   Simon Hengel      2013
License     :  BSD-like

Maintainer  :  haddock@projects.haskell.org
Stability   :  experimental
Portability :  portable

Various utility functions used by the parser.
-}
module Haddock.Parser.Util
  ( takeHorizontalSpace
  , skipHorizontalSpace
  ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Text.Parsec as Parsec
import Prelude hiding (takeWhile)

import Haddock.Parser.Monad

-- | Characters that count as horizontal space
horizontalSpace :: Char -> Bool
horizontalSpace c = isSpace c && c /= '\n'

-- | Skip and ignore leading horizontal space
skipHorizontalSpace :: Parser ()
skipHorizontalSpace = Parsec.skipMany (Parsec.satisfy horizontalSpace)

-- | Take leading horizontal space
takeHorizontalSpace :: Parser Text
takeHorizontalSpace = takeWhile horizontalSpace
