{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      :  Haddock.Types
Copyright   :  (c) Simon Marlow      2003-2006,
                   David Waern       2006-2009,
                   Mateusz Kowalczyk 2013
License     :  BSD-like

Maintainer  :  haddock@projects.haskellorg
Stability   :  experimental
Portability :  portable

Types that are commonly used through-out Haddock. Some of the most
important types are defined here, like 'Interface' and 'DocName'.
-}
module Haddock.Types where

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.CPS (Writer, WriterT, runWriter)
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Writer.Class (MonadWriter (..))
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import Data.Map (Map)
import Data.String (IsString (fromString))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Pollock.CompatGHC as CompatGHC

-----------------------------------------------------------------------------

-- * Convenient synonyms

-----------------------------------------------------------------------------

type DocMap = Map CompatGHC.Name MDoc
type ArgMap = Map CompatGHC.Name (Map Int MDoc)
type DeclMap = Map CompatGHC.Name [CompatGHC.LHsDecl CompatGHC.GhcRn]

type WarningMap = Map CompatGHC.Name Doc

-----------------------------------------------------------------------------

-- * Export items & declarations

-----------------------------------------------------------------------------

data Pollock_ExportDecl name = Pollock_ExportDecl
  { pollock_expItemDecl :: !(CompatGHC.LHsDecl name)
  -- ^ A declaration.
  , pollock_expItemPats :: ![(CompatGHC.HsDecl name, DocForDecl)]
  -- ^ Bundled patterns for a data type declaration
  , -- POL-TODO The Docs for bundled patterns aren't included in the coverage report!!

    pollock_expItemMbDoc :: !(DocForDecl)
  -- ^ Maybe a doc comment, and possibly docs for arguments (if this
  -- decl is a function or type-synonym).
  , pollock_expItemSubDocs :: ![(CompatGHC.IdP name, DocForDecl)]
  -- ^ Subordinate names, possibly with documentation.
  }

-- \| Instances relevant to this declaration, possibly with
-- documentation.
-- , pollock_expItemInstances :: ![DocInstance name]
-- POL-TODO Instances are not used in the coverage report! :( We should look for instances and count them as haddockable...

data Pollock_ExportGroup = Pollock_ExportGroup

newtype Pollock_ExportDoc = Pollock_ExportDoc MDoc
newtype Pollock_ExportModule = Pollock_ExportModule CompatGHC.Module

data Pollock_ExportItem name
  = Pollock_ExportItemDecl (Pollock_ExportDecl name)
  | Pollock_ExportItemNoDecl
  | Pollock_ExportItemGroup Pollock_ExportGroup
  | Pollock_ExportItemDoc Pollock_ExportDoc
  | Pollock_ExportItemModule Pollock_ExportModule

pollock_mkExportGroup :: Pollock_ExportItem name
pollock_mkExportGroup = Pollock_ExportItemGroup Pollock_ExportGroup

pollock_mkExportDoc :: MDoc -> Pollock_ExportItem name
pollock_mkExportDoc = Pollock_ExportItemDoc . Pollock_ExportDoc

data Documentation = Documentation
  { documentationDoc :: Maybe (MDoc)
  , documentationWarning :: !(Maybe Doc)
  }

{- | Arguments and result are indexed by Int, zero-based from the left,
because that's the easiest to use when recursing over types.
-}
type FnArgsDoc = Map Int MDoc

type DocForDecl = (Documentation, FnArgsDoc)

noDocForDecl :: DocForDecl
noDocForDecl = (Documentation Nothing Nothing, mempty)

-----------------------------------------------------------------------------

-- * Documentation comments

-----------------------------------------------------------------------------

type MDoc = MetaDoc

-----------------------------------------------------------------------------

-- * Error handling

-----------------------------------------------------------------------------

-- A monad which collects error messages, locally defined to avoid a dep on mtl

type ErrMsg = BSB.Builder

errMsgFromString :: String -> ErrMsg
errMsgFromString = fromString

errMsgToString :: ErrMsg -> String
errMsgToString = Text.unpack . Text.decodeUtf8 . BSL.toStrict . BSB.toLazyByteString

errMsgUnlines :: [ErrMsg] -> ErrMsg
errMsgUnlines = mconcat . List.intersperse (BSB.charUtf8 '\n')

class (Monad m) => ReportErrorMessage m where
  reportErrorMessage :: BSB.Builder -> m ()

instance (ReportErrorMessage m) => ReportErrorMessage (ReaderT r m) where
  reportErrorMessage = lift . reportErrorMessage

#if !MIN_VERSION_mtl(2,3,0)
-- | @since 2.3
instance (Monoid w, Monad m) => MonadWriter w (CPS.WriterT w m) where
    writer = CPS.writer
    tell   = CPS.tell
    listen = CPS.listen
    pass   = CPS.pass
#endif

instance (Monad m) => ReportErrorMessage (WriterT ErrorMessages m) where
  reportErrorMessage = tell . singleMessage

newtype ErrMsgM a = ErrMsgM {unErrMsgM :: Writer ErrorMessages a}
  deriving newtype (Functor, Applicative, Monad, ReportErrorMessage)

newtype ErrorMessages = ErrorMessages {unErrorMessages :: [BSB.Builder] -> [BSB.Builder]}
  deriving newtype (Semigroup, Monoid)

runErrMsgM :: ErrMsgM a -> (a, ErrorMessages)
runErrMsgM = runWriter . unErrMsgM

singleMessage :: BSB.Builder -> ErrorMessages
singleMessage m = ErrorMessages (m :)

errorMessagesToList :: ErrorMessages -> [BSB.Builder]
errorMessagesToList messages = unErrorMessages messages []

{- | With the advent of 'Version', we may want to start attaching more
meta-data to comments. We make a structure for this ahead of time
so we don't have to gut half the core each time we want to add such
info.
-}
newtype Meta = Meta
  { version :: Maybe Version
  }

emptyMetadata :: Meta
emptyMetadata =
  Meta
    { version = Nothing
    }

data MetaDoc = MetaDoc
  { meta :: Meta
  , doc :: Doc
  }

withEmptyMetadata :: Doc -> MetaDoc
withEmptyMetadata d =
  MetaDoc
    { meta = emptyMetadata
    , doc = d
    }

type Version = [Int]

data Example = Example
  { exampleExpression :: String
  , exampleResult :: [String]
  }

data Doc
  = DocEmpty
  | DocAppend Doc Doc
  | DocString String
  | DocParagraph Doc
  | -- | This constructor has no counterpart in Haddock markup.
    DocWarning Doc
  | DocCodeBlock Doc
  | DocProperty String
  | DocExamples [Example]

data HaddockModInfo = HaddockModInfo
  { hmi_description :: Maybe String
  , hmi_copyright :: Maybe String
  , hmi_license :: Maybe String
  , hmi_maintainer :: Maybe String
  , hmi_stability :: Maybe String
  , hmi_portability :: Maybe String
  , hmi_safety :: Maybe String
  , hmi_language :: Maybe CompatGHC.Language
  , hmi_extensions :: [CompatGHC.Extension]
  }

emptyHaddockModInfo :: HaddockModInfo
emptyHaddockModInfo =
  HaddockModInfo
    { hmi_description = Nothing
    , hmi_copyright = Nothing
    , hmi_license = Nothing
    , hmi_maintainer = Nothing
    , hmi_stability = Nothing
    , hmi_portability = Nothing
    , hmi_safety = Nothing
    , hmi_language = Nothing
    , hmi_extensions = []
    }
