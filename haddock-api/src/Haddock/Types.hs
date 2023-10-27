{-# LANGUAGE CPP, DeriveDataTypeable, DeriveTraversable, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Types
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskellorg
-- Stability   :  experimental
-- Portability :  portable
--
-- Types that are commonly used through-out Haddock. Some of the most
-- important types are defined here, like 'Interface' and 'DocName'.
-----------------------------------------------------------------------------
module Haddock.Types where

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.CPS (Writer, WriterT, runWriter)
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List  as List
import Data.Map (Map)
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Void (Void)

import GHC
import GHC.Types.Name.Occurrence
import GHC.Types.Var (Specificity)

-----------------------------------------------------------------------------
-- * Convenient synonyms
-----------------------------------------------------------------------------


type DocMap a      = Map Name (MDoc a)
type ArgMap a      = Map Name (Map Int (MDoc a))
type DeclMap       = Map Name [LHsDecl GhcRn]

type WarningMap = Map Name (Doc Name)

-----------------------------------------------------------------------------
-- * Export items & declarations
-----------------------------------------------------------------------------

data Pollock_ExportDecl name =
  Pollock_ExportDecl
    {
         -- | A declaration.
        pollock_expItemDecl :: !(LHsDecl name)

        -- | Bundled patterns for a data type declaration
      , pollock_expItemPats :: ![(HsDecl name, DocForDecl (IdP name))]
      -- POL-TODO The Docs for bundled patterns aren't included in the coverage report!!

        -- | Maybe a doc comment, and possibly docs for arguments (if this
        -- decl is a function or type-synonym).
      , pollock_expItemMbDoc :: !(DocForDecl (IdP name))

        -- | Subordinate names, possibly with documentation.
      , pollock_expItemSubDocs :: ![(IdP name, DocForDecl (IdP name))]

        -- | Instances relevant to this declaration, possibly with
        -- documentation.
      -- , pollock_expItemInstances :: ![DocInstance name]
      -- POL-TODO Instances are not used in the coverage report! :( We should look for instances and count them as haddockable...

        -- | Fixity decls relevant to this declaration (including subordinates).
      -- , pollock_expItemFixities :: ![(IdP name, Fixity)]

    }

data Pollock_ExportNoDecl name =
  Pollock_ExportNoDecl
      { pollock_expItemName :: !(IdP name)

        -- | Subordinate names.
      , pollock_expItemSubs :: ![IdP name]
      }

data Pollock_ExportGroup name = Pollock_ExportGroup
      {
        -- | Section level (1, 2, 3, ...).
        pollock_expItemSectionLevel :: !Int

        -- | Section id (for hyperlinks).
      , pollock_expItemSectionId :: !String

        -- | Section heading text.
      , pollock_expItemSectionText :: !(Doc (IdP name))
      }

newtype Pollock_ExportDoc name = Pollock_ExportDoc (MDoc (IdP name))
newtype Pollock_ExportModule = Pollock_ExportModule Module

data Pollock_ExportItem name =
  Pollock_ExportItemDecl (Pollock_ExportDecl name)
  | Pollock_ExportItemNoDecl (Pollock_ExportNoDecl name)
  | Pollock_ExportItemGroup (Pollock_ExportGroup name)
  | Pollock_ExportItemDoc (Pollock_ExportDoc name)
  | Pollock_ExportItemModule Pollock_ExportModule

pollock_mkExportGroup :: Int -> String -> Doc (IdP name) -> Pollock_ExportItem name
pollock_mkExportGroup sectionLevel sectionId = Pollock_ExportItemGroup . Pollock_ExportGroup sectionLevel sectionId

pollock_mkExportDoc :: MDoc (IdP name) -> Pollock_ExportItem name
pollock_mkExportDoc = Pollock_ExportItemDoc . Pollock_ExportDoc

data Documentation name = Documentation
  { documentationDoc :: Maybe (MDoc name)
  , documentationWarning :: !(Maybe (Doc name))
  }


-- | Arguments and result are indexed by Int, zero-based from the left,
-- because that's the easiest to use when recursing over types.
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

-- | Extends 'Name' with cross-reference information.
data DocName
  = Documented Name Module
     -- ^ This thing is part of the (existing or resulting)
     -- documentation. The 'Module' is the preferred place
     -- in the documentation to refer to.
  | Undocumented Name
     -- ^ This thing is not part of the (existing or resulting)
     -- documentation, as far as Haddock knows.

data DocNameI

type instance NoGhcTc DocNameI = DocNameI

type instance IdP DocNameI = DocName

-- | Adds extra "wrapper" information to a name.
--
-- This is to work around the fact that most name types in GHC ('Name', 'RdrName',
-- 'OccName', ...) don't include backticks or parens.
data Wrap n
  = Unadorned { unwrap :: n  }     -- ^ don't do anything to the name
  | Parenthesized { unwrap :: n }  -- ^ add parentheses around the name
  | Backticked { unwrap :: n }     -- ^ add backticks around the name
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
-- * Options
-----------------------------------------------------------------------------


-- | Source-level options for controlling the documentation.
data DocOption
  = OptHide            -- ^ This module should not appear in the docs.
  | OptPrune
  | OptIgnoreExports   -- ^ Pretend everything is exported.
  | OptNotHome         -- ^ Not the best place to get docs for things
                       -- exported by this module.
  | OptShowExtensions  -- ^ Render enabled extensions for this module.
  deriving (Eq)

-----------------------------------------------------------------------------
-- * Error handling
-----------------------------------------------------------------------------


-- A monad which collects error messages, locally defined to avoid a dep on mtl


type ErrMsg = Builder

errMsgFromString :: String -> ErrMsg
errMsgFromString = fromString

errMsgToString :: ErrMsg -> String
errMsgToString = Text.unpack . Text.decodeUtf8 . BSL.toStrict . toLazyByteString

errMsgUnlines :: [ErrMsg] -> ErrMsg
errMsgUnlines = mconcat . List.intersperse (charUtf8 '\n')

class Monad m => ReportErrorMessage m where
    reportErrorMessage :: Builder -> m ()

instance ReportErrorMessage m => ReportErrorMessage (ReaderT r m) where
    reportErrorMessage = lift . reportErrorMessage

#if !MIN_VERSION_mtl(2,3,0)
-- | @since 2.3
instance (Monoid w, Monad m) => MonadWriter w (CPS.WriterT w m) where
    writer = CPS.writer
    tell   = CPS.tell
    listen = CPS.listen
    pass   = CPS.pass
#endif

instance Monad m => ReportErrorMessage (WriterT ErrorMessages m) where
    reportErrorMessage = tell . singleMessage

newtype ErrMsgM a = ErrMsgM { unErrMsgM :: Writer ErrorMessages a }
    deriving newtype (Functor, Applicative, Monad, ReportErrorMessage)

newtype ErrorMessages = ErrorMessages { unErrorMessages :: [Builder] -> [Builder] }
    deriving newtype (Semigroup, Monoid)

runErrMsgM :: ErrMsgM a -> (a, ErrorMessages)
runErrMsgM = runWriter . unErrMsgM

singleMessage :: Builder -> ErrorMessages
singleMessage m = ErrorMessages $ (m :)

errorMessagesToList :: ErrorMessages -> [Builder]
errorMessagesToList messages = unErrorMessages messages []

-----------------------------------------------------------------------------
-- * Pass sensitive types
-----------------------------------------------------------------------------

type instance XRec DocNameI a = GenLocated (Anno a) a

type instance Anno DocName                           = SrcSpanAnnN
type instance Anno (HsTyVarBndr flag DocNameI)       = SrcSpanAnnA
type instance Anno [LocatedA (HsType DocNameI)]      = SrcSpanAnnC
type instance Anno (HsType DocNameI)                 = SrcSpanAnnA
type instance Anno (DataFamInstDecl DocNameI)        = SrcSpanAnnA
type instance Anno (DerivStrategy DocNameI)          = SrcAnn NoEpAnns
type instance Anno (FieldOcc DocNameI)               = SrcAnn NoEpAnns
type instance Anno (ConDeclField DocNameI)           = SrcSpan
type instance Anno (Located (ConDeclField DocNameI)) = SrcSpan
type instance Anno [Located (ConDeclField DocNameI)] = SrcSpan
type instance Anno (ConDecl DocNameI)                = SrcSpan
type instance Anno (FunDep DocNameI)                 = SrcSpan
type instance Anno (TyFamInstDecl DocNameI)          = SrcSpanAnnA
type instance Anno [LocatedA (TyFamInstDecl DocNameI)] = SrcSpanAnnL
type instance Anno (FamilyDecl DocNameI)               = SrcSpan
type instance Anno (Sig DocNameI)                      = SrcSpan
type instance Anno (InjectivityAnn DocNameI)           = SrcAnn NoEpAnns
type instance Anno (HsDecl DocNameI)                   = SrcSpanAnnA
type instance Anno (FamilyResultSig DocNameI)          = SrcAnn NoEpAnns
type instance Anno (HsOuterTyVarBndrs Specificity DocNameI) = SrcSpanAnnA
type instance Anno (HsSigType DocNameI)                     = SrcSpanAnnA

type instance XForAllTy        DocNameI = EpAnn [AddEpAnn]
type instance XQualTy          DocNameI = EpAnn [AddEpAnn]
type instance XTyVar           DocNameI = EpAnn [AddEpAnn]
type instance XStarTy          DocNameI = EpAnn [AddEpAnn]
type instance XAppTy           DocNameI = EpAnn [AddEpAnn]
type instance XAppKindTy       DocNameI = EpAnn [AddEpAnn]
type instance XFunTy           DocNameI = EpAnn [AddEpAnn]
type instance XListTy          DocNameI = EpAnn AnnParen
type instance XTupleTy         DocNameI = EpAnn AnnParen
type instance XSumTy           DocNameI = EpAnn AnnParen
type instance XOpTy            DocNameI = EpAnn [AddEpAnn]
type instance XParTy           DocNameI = EpAnn AnnParen
type instance XIParamTy        DocNameI = EpAnn [AddEpAnn]
type instance XKindSig         DocNameI = EpAnn [AddEpAnn]
type instance XSpliceTy        DocNameI = Void       -- see `renameHsSpliceTy`
type instance XDocTy           DocNameI = EpAnn [AddEpAnn]
type instance XBangTy          DocNameI = EpAnn [AddEpAnn]
type instance XRecTy           DocNameI = EpAnn [AddEpAnn]
type instance XExplicitListTy  DocNameI = EpAnn [AddEpAnn]
type instance XExplicitTupleTy DocNameI = EpAnn [AddEpAnn]
type instance XTyLit           DocNameI = EpAnn [AddEpAnn]
type instance XWildCardTy      DocNameI = EpAnn [AddEpAnn]
type instance XXType           DocNameI = HsCoreTy

type instance XHsForAllVis        DocNameI = NoExtField
type instance XHsForAllInvis      DocNameI = NoExtField
type instance XXHsForAllTelescope DocNameI = DataConCantHappen

type instance XUserTyVar    DocNameI = NoExtField
type instance XKindedTyVar  DocNameI = NoExtField
type instance XXTyVarBndr   DocNameI = DataConCantHappen

type instance XCFieldOcc   DocNameI = DocName
type instance XXFieldOcc   DocNameI = NoExtField

type instance XFixitySig   DocNameI = NoExtField
type instance XFixSig      DocNameI = NoExtField
type instance XPatSynSig   DocNameI = NoExtField
type instance XClassOpSig  DocNameI = NoExtField
type instance XTypeSig     DocNameI = NoExtField
type instance XMinimalSig  DocNameI = NoExtField

type instance XForeignExport  DocNameI = NoExtField
type instance XForeignImport  DocNameI = NoExtField
type instance XConDeclGADT    DocNameI = NoExtField
type instance XConDeclH98     DocNameI = NoExtField
type instance XXConDecl       DocNameI = DataConCantHappen

type instance XDerivD     DocNameI = NoExtField
type instance XInstD      DocNameI = NoExtField
type instance XForD       DocNameI = NoExtField
type instance XSigD       DocNameI = NoExtField
type instance XTyClD      DocNameI = NoExtField

type instance XNoSig            DocNameI = NoExtField
type instance XCKindSig         DocNameI = NoExtField
type instance XTyVarSig         DocNameI = NoExtField
type instance XXFamilyResultSig DocNameI = DataConCantHappen

type instance XCFamEqn       DocNameI _ = NoExtField
type instance XXFamEqn       DocNameI _ = DataConCantHappen

type instance XCClsInstDecl DocNameI = NoExtField
type instance XCDerivDecl   DocNameI = NoExtField
type instance XStockStrategy    DocNameI = NoExtField
type instance XAnyClassStrategy DocNameI = NoExtField
type instance XNewtypeStrategy  DocNameI = NoExtField
type instance XViaStrategy  DocNameI = LHsSigType DocNameI
type instance XDataFamInstD DocNameI = NoExtField
type instance XTyFamInstD   DocNameI = NoExtField
type instance XClsInstD     DocNameI = NoExtField
type instance XCHsDataDefn  DocNameI = NoExtField
type instance XCFamilyDecl  DocNameI = NoExtField
type instance XClassDecl    DocNameI = NoExtField
type instance XDataDecl     DocNameI = NoExtField
type instance XSynDecl      DocNameI = NoExtField
type instance XFamDecl      DocNameI = NoExtField
type instance XXFamilyDecl  DocNameI = DataConCantHappen
type instance XXTyClDecl    DocNameI = DataConCantHappen

type instance XHsWC DocNameI _ = NoExtField

type instance XHsOuterExplicit    DocNameI _ = NoExtField
type instance XHsOuterImplicit    DocNameI   = NoExtField
type instance XXHsOuterTyVarBndrs DocNameI   = DataConCantHappen

type instance XHsSig      DocNameI = NoExtField
type instance XXHsSigType DocNameI = DataConCantHappen

type instance XHsQTvs        DocNameI = NoExtField
type instance XConDeclField  DocNameI = NoExtField
type instance XXConDeclField DocNameI = DataConCantHappen

type instance XXPat DocNameI = DataConCantHappen
type instance XXHsBindsLR DocNameI a = DataConCantHappen

type instance XCInjectivityAnn DocNameI = NoExtField

type instance XCFunDep DocNameI = NoExtField

type instance XCTyFamInstDecl DocNameI = NoExtField

-- | With the advent of 'Version', we may want to start attaching more
-- meta-data to comments. We make a structure for this ahead of time
-- so we don't have to gut half the core each time we want to add such
-- info.
data Meta = Meta { _version :: Maybe Version
                 , _package :: Maybe Package
                 }

data MetaDoc mod id =
  MetaDoc { _meta :: Meta
          , _doc :: DocH mod id
          }

overDoc :: (DocH a b -> DocH c d) -> MetaDoc a b -> MetaDoc c d
overDoc f d = d { _doc = f $ _doc d }

overDocF :: Functor f => (DocH a b -> f (DocH c d)) -> MetaDoc a b -> f (MetaDoc c d)
overDocF f d = (\x -> d { _doc = x }) <$> f (_doc d)

type Version = [Int]
type Package = String

data Hyperlink id = Hyperlink
  { hyperlinkUrl   :: String
  , hyperlinkLabel :: Maybe id
  }

data ModLink id = ModLink
  { modLinkName   :: String
  , modLinkLabel :: Maybe id
  }

data Picture = Picture
  { pictureUri   :: String
  , pictureTitle :: Maybe String
  }

data Header id = Header
  { headerLevel :: Int  -- ^ between 1 and 6 inclusive
  , headerTitle :: id
  }

data Example = Example
  { exampleExpression :: String
  , exampleResult     :: [String]
  }

data TableCell id = TableCell
  { tableCellColspan  :: Int
  , tableCellRowspan  :: Int
  , tableCellContents :: id
  } deriving (Functor, Foldable, Traversable)

newtype TableRow id = TableRow
  { tableRowCells :: [TableCell id]
  } deriving (Functor, Foldable, Traversable)

data Table id = Table
  { tableHeaderRows :: [TableRow id]
  , tableBodyRows   :: [TableRow id]
  } deriving (Functor, Foldable, Traversable)

data DocH mod id
  = DocEmpty
  | DocAppend (DocH mod id) (DocH mod id)
  | DocString String
  | DocParagraph (DocH mod id)
  | DocIdentifier id
  | DocIdentifierUnchecked mod
  -- ^ A qualified identifier that couldn't be resolved.
  | DocModule (ModLink (DocH mod id))
  -- ^ A link to a module, with an optional label.
  | DocWarning (DocH mod id)
  -- ^ This constructor has no counterpart in Haddock markup.
  | DocEmphasis (DocH mod id)
  | DocMonospaced (DocH mod id)
  | DocBold (DocH mod id)
  | DocUnorderedList [DocH mod id]
  | DocOrderedList [(Int, DocH mod id)]
  | DocDefList [(DocH mod id, DocH mod id)]
  | DocCodeBlock (DocH mod id)
  | DocHyperlink (Hyperlink (DocH mod id))
  | DocPic Picture
  | DocMathInline String
  | DocMathDisplay String
  | DocAName String
  -- ^ A (HTML) anchor. It must not contain any spaces.
  | DocProperty String
  | DocExamples [Example]
  | DocHeader (Header (DocH mod id))
  | DocTable (Table (DocH mod id))

-- | The namespace qualification for an identifier.
data Namespace = Value | Type | None

-- | Render the a namespace into the same format it was initially parsed.
renderNs :: Namespace -> String
renderNs Value = "v"
renderNs Type = "t"
renderNs None = ""


-- | 'DocMarkupH' is a set of instructions for marking up documentation.
-- In fact, it's really just a mapping from 'Doc' to some other
-- type [a], where [a] is usually the type of the output (HTML, say).
-- Use 'Documentation.Haddock.Markup.markup' to apply a 'DocMarkupH' to
-- a 'DocH'.
--
-- @since 1.4.5
--
data DocMarkupH mod id a = Markup
  { markupEmpty                :: a
  , markupString               :: String -> a
  , markupParagraph            :: a -> a
  , markupAppend               :: a -> a -> a
  , markupIdentifier           :: id -> a
  , markupIdentifierUnchecked  :: mod -> a
  , markupModule               :: ModLink a -> a
  , markupWarning              :: a -> a
  , markupEmphasis             :: a -> a
  , markupBold                 :: a -> a
  , markupMonospaced           :: a -> a
  , markupUnorderedList        :: [a] -> a
  , markupOrderedList          :: [(Int,a)] -> a
  , markupDefList              :: [(a,a)] -> a
  , markupCodeBlock            :: a -> a
  , markupHyperlink            :: Hyperlink a -> a
  , markupAName                :: String -> a
  , markupPic                  :: Picture -> a
  , markupMathInline           :: String -> a
  , markupMathDisplay          :: String -> a
  , markupProperty             :: String -> a
  , markupExample              :: [Example] -> a
  , markupHeader               :: Header a -> a
  , markupTable                :: Table a -> a
  }
