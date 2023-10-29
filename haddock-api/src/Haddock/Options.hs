-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Options
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Definition of the command line interface of Haddock.
-----------------------------------------------------------------------------
module Haddock.Options (
  modulePackageInfo
) where


import Data.Version ( Version )
import GHC ( moduleUnit, Module )
import GHC.Unit.State
    ( lookupUnit,
      PackageName,
      UnitState,
      GenericUnitInfo(unitPackageVersion, unitPackageName) )


-- | This function has a potential to return 'Nothing' because package name and
-- versions can no longer reliably be extracted in all cases: if the package is
-- not installed yet then this info is no longer available.
--
-- The @--package-name@ and @--package-version@ Haddock flags allow the user to
-- specify this information manually and it is returned here if present.
modulePackageInfo :: UnitState
                  -> Maybe Module
                  -> (Maybe PackageName, Maybe Data.Version.Version)
modulePackageInfo _unit_state Nothing = (Nothing, Nothing)
modulePackageInfo unit_state (Just modu) =
  (  fmap unitPackageName pkgDb
  ,  fmap unitPackageVersion pkgDb
  )
  where
    pkgDb = lookupUnit unit_state (moduleUnit modu)
