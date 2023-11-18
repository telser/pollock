{- |
Module: Pollock
Copyright: (c) Trevis Elser 2023
License: MIT

Maintainer: trevis@flipstone.com
Stability: experimental
-}
module Pollock
  ( processModule
  , ModuleHeader (..)
  , ModuleInfo (..)
  , ensureHaddockIsOn
  ) where

import Pollock.DriverPlugin (ensureHaddockIsOn)
import Pollock.ModuleInfo (ModuleHeader (..), ModuleInfo (..))
import Pollock.ProcessModule (processModule)
