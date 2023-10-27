-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Utils
-- Copyright   :  (c) The University of Glasgow 2001-2002,
--                    Simon Marlow 2003-2006,
--                    David Waern  2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Utils (

  -- * Miscellaneous utilities
  die,

  -- * List utilities
  replace,

  -- * Logging
  Verbosity(..),

 ) where

import System.Exit

--------------------------------------------------------------------------------
-- * Logging
--------------------------------------------------------------------------------

data Verbosity = Silent | Normal | Verbose | Deafening
  deriving (Eq, Ord, Enum, Bounded, Show)

-----------------------------------------------------------------------------
-- * List utils
-----------------------------------------------------------------------------


replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map (\x -> if x == a then b else x)
