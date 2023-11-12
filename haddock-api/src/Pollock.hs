{- |
Module      :  Pollock
Copyright   :  (c) Trevis Elser 2023
License     :  MIT

Maintainer  :  trevis@flipstone.com
Stability   :  experimental
-}
module Pollock (processModule, ModuleHeader (..), ensureHaddockIsOn) where

import Pollock.DriverPlugin (ensureHaddockIsOn)
import Pollock.ModuleInfo (ModuleHeader (..))
import Pollock.ProcessModule (processModule)
