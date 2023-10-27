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
  Flag(..),
  Visibility(..),
  optPackageName,
  optPackageVersion,
  modulePackageInfo
) where


import           Control.Applicative
import           Data.Version
import           GHC ( Module, moduleUnit )
import           GHC.Data.FastString
import           GHC.Unit.State
import qualified Text.ParserCombinators.ReadP as RP


data Flag
  = Flag_BuiltInThemes
  | Flag_CSS String
--  | Flag_DocBook
  | Flag_ReadInterface String
  | Flag_DumpInterface String
  | Flag_ShowInterface String
  | Flag_Heading String
  | Flag_Html
  | Flag_Hoogle
  | Flag_Lib String
  | Flag_OutputDir FilePath
  | Flag_Prologue FilePath
  | Flag_SourceBaseURL    String
  | Flag_SourceModuleURL  String
  | Flag_SourceEntityURL  String
  | Flag_SourceLEntityURL String
  | Flag_WikiBaseURL   String
  | Flag_BaseURL       String
  | Flag_WikiModuleURL String
  | Flag_WikiEntityURL String
  | Flag_LaTeX
  | Flag_LaTeXStyle String
  | Flag_QuickJumpIndex
  | Flag_HyperlinkedSource
  | Flag_SourceCss String
  | Flag_Mathjax String
  | Flag_Help
  | Flag_Verbosity String
  | Flag_Version
  | Flag_CompatibleInterfaceVersions
  | Flag_InterfaceVersion
  | Flag_BypassInterfaceVersonCheck
  | Flag_UseContents String
  | Flag_GenContents
  | Flag_UseIndex String
  | Flag_GenIndex
  | Flag_IgnoreAllExports
  | Flag_HideModule String
  | Flag_ShowModule String
  | Flag_ShowAllModules
  | Flag_ShowExtensions String
  | Flag_OptGhc String
  | Flag_GhcLibDir String
  | Flag_GhcVersion
  | Flag_PrintGhcPath
  | Flag_PrintGhcLibDir
  | Flag_NoWarnings
  | Flag_UseUnicode
  | Flag_NoTmpCompDir
  | Flag_Qualification String
  | Flag_PrettyHtml
  | Flag_NoPrintMissingDocs
  | Flag_PackageName String
  | Flag_PackageVersion String
  | Flag_Reexport String
  | Flag_SinceQualification String
  | Flag_IgnoreLinkSymbol String
  | Flag_ParCount (Maybe Int)
  deriving (Eq)

optPackageVersion :: [Flag] -> Maybe Data.Version.Version
optPackageVersion flags =
  let ver = optLast [ v | Flag_PackageVersion v <- flags ]
  in ver >>= fmap fst . optLast . RP.readP_to_S parseVersion

optPackageName :: [Flag] -> Maybe PackageName
optPackageName flags =
  optLast [ PackageName $ mkFastString n | Flag_PackageName n <- flags ]

data Visibility = Visible | Hidden


-- | Like 'listToMaybe' but returns the last element instead of the first.
optLast :: [a] -> Maybe a
optLast [] = Nothing
optLast xs = Just (last xs)


-- | This function has a potential to return 'Nothing' because package name and
-- versions can no longer reliably be extracted in all cases: if the package is
-- not installed yet then this info is no longer available.
--
-- The @--package-name@ and @--package-version@ Haddock flags allow the user to
-- specify this information manually and it is returned here if present.
modulePackageInfo :: UnitState
                  -> [Flag] -- ^ Haddock flags are checked as they may contain
                            -- the package name or version provided by the user
                            -- which we prioritise
                  -> Maybe Module
                  -> (Maybe PackageName, Maybe Data.Version.Version)
modulePackageInfo _unit_state _flags Nothing = (Nothing, Nothing)
modulePackageInfo unit_state flags (Just modu) =
  ( optPackageName flags    <|> fmap unitPackageName pkgDb
  , optPackageVersion flags <|> fmap unitPackageVersion pkgDb
  )
  where
    pkgDb = lookupUnit unit_state (moduleUnit modu)
