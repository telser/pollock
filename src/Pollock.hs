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
import Pollock.ModuleInfo
  ( ModuleHeader
      ( ModuleHeader
      , copyright
      , description
      , license
      , maintainer
      , portability
      , stability
      )
  , ModuleInfo
    ( ModuleInfo
    , haddockableExports
    , haddockedExports
    , moduleHeader
    , numWithCodeBlock
    , numWithExample
    , numWithProperty
    , numWithSince
    , numWithWarning
    )
  )
import Pollock.ProcessModule (processModule)
