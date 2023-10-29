{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

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

import GHC
  ( GhcRn
  , HsDecl
  , IdP
  , LHsDecl
  , Module
  , ModuleName
  , Name
  , RdrName
  )
import GHC.Driver.Session (Language)
import qualified GHC.LanguageExtensions as LangExt
import GHC.Types.Name.Occurrence (OccName)

-----------------------------------------------------------------------------

-- * Convenient synonyms

-----------------------------------------------------------------------------

type DocMap a = Map Name (MDoc a)
type ArgMap a = Map Name (Map Int (MDoc a))
type DeclMap = Map Name [LHsDecl GhcRn]

type WarningMap = Map Name (Doc Name)

-----------------------------------------------------------------------------

-- * Export items & declarations

-----------------------------------------------------------------------------

data Pollock_ExportDecl name = Pollock_ExportDecl
  { pollock_expItemDecl :: !(LHsDecl name)
  -- ^ A declaration.
  , pollock_expItemPats :: ![(HsDecl name, DocForDecl (IdP name))]
  -- ^ Bundled patterns for a data type declaration
  , -- POL-TODO The Docs for bundled patterns aren't included in the coverage report!!

    pollock_expItemMbDoc :: !(DocForDecl (IdP name))
  -- ^ Maybe a doc comment, and possibly docs for arguments (if this
  -- decl is a function or type-synonym).
  , pollock_expItemSubDocs :: ![(IdP name, DocForDecl (IdP name))]
  -- ^ Subordinate names, possibly with documentation.
  }

-- \| Instances relevant to this declaration, possibly with
-- documentation.
-- , pollock_expItemInstances :: ![DocInstance name]
-- POL-TODO Instances are not used in the coverage report! :( We should look for instances and count them as haddockable...

-- \| Fixity decls relevant to this declaration (including subordinates).
-- , pollock_expItemFixities :: ![(IdP name, Fixity)]

data Pollock_ExportNoDecl name = Pollock_ExportNoDecl
  { pollock_expItemName :: !(IdP name)
  , pollock_expItemSubs :: ![IdP name]
  -- ^ Subordinate names.
  }

data Pollock_ExportGroup name = Pollock_ExportGroup
  { pollock_expItemSectionLevel :: !Int
  -- ^ Section level (1, 2, 3, ...).
  , pollock_expItemSectionText :: !(Doc (IdP name))
  -- ^ Section heading text.
  }

newtype Pollock_ExportDoc name = Pollock_ExportDoc (MDoc (IdP name))
newtype Pollock_ExportModule = Pollock_ExportModule Module

data Pollock_ExportItem name
  = Pollock_ExportItemDecl (Pollock_ExportDecl name)
  | Pollock_ExportItemNoDecl (Pollock_ExportNoDecl name)
  | Pollock_ExportItemGroup (Pollock_ExportGroup name)
  | Pollock_ExportItemDoc (Pollock_ExportDoc name)
  | Pollock_ExportItemModule Pollock_ExportModule

pollock_mkExportGroup :: Int -> Doc (IdP name) -> Pollock_ExportItem name
pollock_mkExportGroup sectionLevel = Pollock_ExportItemGroup . Pollock_ExportGroup sectionLevel

pollock_mkExportDoc :: MDoc (IdP name) -> Pollock_ExportItem name
pollock_mkExportDoc = Pollock_ExportItemDoc . Pollock_ExportDoc

data Documentation name = Documentation
  { documentationDoc :: Maybe (MDoc name)
  , documentationWarning :: !(Maybe (Doc name))
  }

{- | Arguments and result are indexed by Int, zero-based from the left,
because that's the easiest to use when recursing over types.
-}
type FnArgsDoc name = Map Int (MDoc name)

type DocForDecl name = (Documentation name, FnArgsDoc name)

noDocForDecl :: DocForDecl name
noDocForDecl = (Documentation Nothing Nothing, mempty)

-----------------------------------------------------------------------------

-- * Cross-referencing

-----------------------------------------------------------------------------

-- | An 'RdrName' tagged with some type/value namespace information.
data NsRdrName = NsRdrName
  { namespace :: !Namespace
  , rdrName :: !RdrName
  }

{- | Adds extra "wrapper" information to a name.

This is to work around the fact that most name types in GHC ('Name', 'RdrName',
'OccName', ...) don't include backticks or parens.
-}
data Wrap n
  = -- | don't do anything to the name
    Unadorned {unwrap :: n}
  | -- | add parentheses around the name
    Parenthesized {unwrap :: n}
  | -- | add backticks around the name
    Backticked {unwrap :: n}
  deriving (Functor)

showWrapped :: (a -> String) -> Wrap a -> String
showWrapped f (Unadorned n) = f n
showWrapped f (Parenthesized n) = "(" ++ f n ++ ")"
showWrapped f (Backticked n) = "`" ++ f n ++ "`"

-----------------------------------------------------------------------------

-- * Documentation comments

-----------------------------------------------------------------------------

type Doc id = DocH (Wrap (ModuleName, OccName)) (Wrap id)
type MDoc id = MetaDoc (Wrap (ModuleName, OccName)) (Wrap id)

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
data Meta = Meta
  { _version :: Maybe Version
  , _package :: Maybe Package
  }

data MetaDoc mod id = MetaDoc
  { _meta :: Meta
  , _doc :: DocH mod id
  }

overDoc :: (DocH a b -> DocH c d) -> MetaDoc a b -> MetaDoc c d
overDoc f d = d{_doc = f $ _doc d}

overDocF :: (Functor f) => (DocH a b -> f (DocH c d)) -> MetaDoc a b -> f (MetaDoc c d)
overDocF f d = (\x -> d{_doc = x}) <$> f (_doc d)

type Version = [Int]
type Package = String

data Example = Example
  { exampleExpression :: String
  , exampleResult :: [String]
  }

data DocH mod id
  = DocEmpty
  | DocAppend (DocH mod id) (DocH mod id)
  | DocString String
  | DocParagraph (DocH mod id)
  | DocIdentifier id
  | -- | A qualified identifier that couldn't be resolved.
    DocIdentifierUnchecked mod
  | -- | This constructor has no counterpart in Haddock markup.
    DocWarning (DocH mod id)
  | DocCodeBlock (DocH mod id)
  | DocProperty String
  | DocExamples [Example]

-- | The namespace qualification for an identifier.
data Namespace = Value | Type | None

-- | Render the a namespace into the same format it was initially parsed.
renderNs :: Namespace -> String
renderNs Value = "v"
renderNs Type = "t"
renderNs None = ""

data HaddockModInfo name = HaddockModInfo
  { hmi_description :: Maybe (Doc name)
  , hmi_copyright :: Maybe String
  , hmi_license :: Maybe String
  , hmi_maintainer :: Maybe String
  , hmi_stability :: Maybe String
  , hmi_portability :: Maybe String
  , hmi_safety :: Maybe String
  , hmi_language :: Maybe Language
  , hmi_extensions :: [LangExt.Extension]
  }

emptyHaddockModInfo :: HaddockModInfo a
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
