module CompatGHC
  ( ClassMinimalDef, classMinimalDef
  , ConLike (..)
  , unpackFS
  , HscEnv(hsc_unit_env)
  , showSDoc
  , readIORef
  , HasCallStack
  , TcGblEnv(tcg_exports, tcg_insts, tcg_fam_insts, tcg_src,
               tcg_warns, tcg_th_docs, tcg_mod, tcg_semantic_mod, tcg_rdr_env,
               tcg_doc_hdr, tcg_rn_imports, tcg_rn_decls, tcg_rn_exports)
  , finalSafeMode
  , getOccString, getSrcSpan, isDataConName, isValName, nameIsLocalOrFrom, nameOccName, emptyOccEnv
  , PromotionFlag (..)
  , lookupNameEnv
  , GlobalRdrEnv, greMangledName, lookupGlobalRdrEnv
  , elemNameSet, mkNameSet
  , HscSource (..)
  , SourceText (..), sl_fs
  , nonDetEltsUniqMap
  , ue_units
  , Warnings(..), WarningTxt(..)
  , PackageName (..)
  , pprPanic
  , declTypeDocs,
      extractTHDocs,
      getInstLoc,
      getMainDeclBinder,
      isValD,
      nubByName,
      subordinates,
      topDecls
  ) where

import GHC.Core.Class (ClassMinimalDef, classMinimalDef)
import GHC.Core.ConLike (ConLike (..))
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Env ( HscEnv(hsc_unit_env) )
import GHC.Driver.Ppr (showSDoc)

import GHC.HsToCore.Docs( declTypeDocs,      extractTHDocs,      getInstLoc,      getMainDeclBinder,      isValD,      nubByName,      subordinates,      topDecls )
import GHC.IORef (readIORef)
import GHC.Stack (HasCallStack)
import GHC.Tc.Types
    ( TcGblEnv(tcg_exports, tcg_insts, tcg_fam_insts, tcg_src,
               tcg_warns, tcg_th_docs, tcg_mod, tcg_semantic_mod, tcg_rdr_env,
               tcg_doc_hdr, tcg_rn_imports, tcg_rn_decls, tcg_rn_exports) )
import GHC.Tc.Utils.Monad (finalSafeMode)
import GHC.Types.Basic (PromotionFlag (..))
import GHC.Types.Name (getOccString, getSrcSpan, isDataConName, isValName, nameIsLocalOrFrom, nameOccName, emptyOccEnv)
import GHC.Types.Name.Env (lookupNameEnv)
import GHC.Types.Name.Reader (GlobalRdrEnv, greMangledName, lookupGlobalRdrEnv)
import GHC.Types.Name.Set (elemNameSet, mkNameSet)
import GHC.Types.SourceFile (HscSource (..))
import GHC.Types.SourceText (SourceText (..), sl_fs)
import GHC.Types.Unique.Map ( nonDetEltsUniqMap )
import GHC.Unit.Env ( ue_units )
import GHC.Unit.Module.Warnings ( Warnings(..), WarningTxt(..) )
import GHC.Unit.State (PackageName (..))
import GHC.Utils.Panic (pprPanic)
