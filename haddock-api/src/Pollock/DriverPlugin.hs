module Pollock.DriverPlugin
  ( ensureHaddockIsOn
  ) where

import qualified Pollock.CompatGHC as CompatGHC

{- | A helper suitable for use to set as 'driverPlugin' that ensures the Haddock option is set to
allow other funcationality provided here to work.
-}
ensureHaddockIsOn :: [a] -> CompatGHC.HscEnv -> IO CompatGHC.HscEnv
ensureHaddockIsOn _ env =
  let
    dflags = CompatGHC.hsc_dflags env
    newDflags =
      dflags
        { CompatGHC.generalFlags =
            CompatGHC.insertEnumSet CompatGHC.Opt_Haddock (CompatGHC.generalFlags dflags)
        }
   in
    pure $
      env
        { CompatGHC.hsc_dflags = newDflags
        }
