{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module: Text.Ascii.Char
-- Copyright: (C) 2021 Koz Ross
-- License: Apache 2.0
-- Maintainer: Koz Ross <koz.ross@retro-freedom.nz>
-- Stability: stable
-- Portability: GHC only
--
-- An implementation of ASCII characters, as bytes restricted to the range 0 -
-- 127 inclusive.
--
-- /See also:/ [Wikipedia entry for ASCII](https://en.wikipedia.org/wiki/ASCII)
module Text.Ascii.Char
  ( -- * ASCII characters

    -- ** Type
    AsciiChar (AsByte, AsChar),

    -- ** Construction
    char,
    fromChar,
    fromByte,

    -- ** Transformation
    upcase,
    downcase,

    -- * Categorization
    AsciiType (Control, Printable),
    charType,
    AsciiCategory (Other, Punctuation, Letter, Number, Symbol),
    categorize,
    categorizeGeneral,
    AsciiCase (Upper, Lower),
    caseOf,

    -- * Optics
    charWise,
    byteWise,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Char (GeneralCategory, chr, generalCategory, isAscii, ord)
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.Word (Word8)
import Optics.Prism (Prism', prism')
import Text.Ascii.Internal (AsciiChar (AsciiChar), toByte, pattern AsByte, pattern AsChar)
import Text.Ascii.QQ (char)

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Text.Ascii.Char

-- | Try and turn a 'Char' into the equivalent 'AsciiChar'. Will return
-- 'Nothing' if given a 'Char' that has no ASCII equivalent.
--
-- >>> fromChar '0'
-- Just '0x30'
-- >>> fromChar '😺'
-- Nothing
--
-- @since 1.0.0
fromChar :: Char -> Maybe AsciiChar
fromChar c =
  if isAscii c
    then pure . AsciiChar . fromIntegral . ord $ c
    else Nothing

-- | Try to give the 'AsciiChar' corresponding to the given byte. Will return
-- 'Nothing' if given a byte that doesn't correspond to an ASCII character.
--
-- >>> fromByte 50
-- Just '0x32'
-- >>> fromByte 128
-- Nothing
--
-- @since 1.0.0
fromByte :: Word8 -> Maybe AsciiChar
fromByte w8 =
  if isAscii . chr . fromIntegral $ w8
    then pure . AsciiChar $ w8
    else Nothing

-- | Give the 'AsciiChar' corresponding to the uppercase version of the
-- argument. Will give 'Nothing' if given an 'AsciiChar' which has no uppercase
-- version, or is uppercase already.
--
-- >>> upcase [char| 'a' |]
-- Just '0x41'
-- >>> upcase [char| '0' |]
-- Nothing
--
-- @since 1.0.0
upcase :: AsciiChar -> Maybe AsciiChar
upcase c@(AsciiChar w8) =
  caseOf c >>= (\cs -> guard (cs == Lower) $> AsciiChar (w8 - 32))

-- | Give the 'AsciiChar' corresponding to the lowercase version of the
-- argument. Will give 'Nothing' if given an 'AsciiChar' which has no lowercase
-- version, or is lowercase already.
--
-- >>> downcase [char| 'C' |]
-- Just '0x63'
-- >>> downcase [char| '\\' |]
-- Nothing
--
-- @since 1.0.0
downcase :: AsciiChar -> Maybe AsciiChar
downcase c@(AsciiChar w8) =
  caseOf c >>= (\cs -> guard (cs == Upper) $> AsciiChar (w8 + 32))

-- Categorization

-- | A categorization of ASCII characters based on whether they're meant to be
-- displayed ('Printable') or for control ('Control').
--
-- @since 1.0.0
newtype AsciiType = AsciiType Word8
  deriving (Eq, Ord, Hashable, NFData) via Word8

-- | @since 1.0.0
instance Show AsciiType where
  {-# INLINEABLE show #-}
  show = \case
    Control -> "Control"
    Printable -> "Printable"

-- | @since 1.0.0
instance Bounded AsciiType where
  minBound = Control
  maxBound = Printable

-- | A control character is any of the first 32 bytes (0-31), plus @DEL@ (127).
--
-- @since 1.0.0
pattern Control :: AsciiType
pattern Control <-
  AsciiType 0
  where
    Control = AsciiType 0

-- | All ASCII characters whose byte is above 31 (and not 127) are printable
-- characters.
--
-- @since 1.0.0
pattern Printable :: AsciiType
pattern Printable <-
  AsciiType 1
  where
    Printable = AsciiType 1

{-# COMPLETE Control, Printable #-}

-- | Classify an 'AsciiChar' according to whether it's a control character or a
-- printable character.
--
-- >>> charType [char| '\0' |]
-- Control
-- >>> charType [char| 'w' |]
-- Printable
--
-- @since 1.0.0
charType :: AsciiChar -> AsciiType
charType (AsciiChar w8)
  | w8 == 127 = Control
  | w8 < 32 = Control
  | otherwise = Printable

-- | A categorization of ASCII characters based on their usage. Based (loosely)
-- on Unicode categories.
--
-- @since 1.0.0
newtype AsciiCategory = AsciiCategory Word8
  deriving (Eq, Ord, Hashable, NFData) via Word8

-- | @since 1.0.0
instance Show AsciiCategory where
  {-# INLINEABLE show #-}
  show = \case
    Other -> "Other"
    Symbol -> "Symbol"
    Number -> "Number"
    Letter -> "Letter"
    Punctuation -> "Punctuation"

-- | @since 1.0.0
instance Bounded AsciiCategory where
  minBound = Other
  maxBound = Symbol

-- | Something which doesn't fit into any of the other categories.
--
-- @since 1.0.0
pattern Other :: AsciiCategory
pattern Other <-
  AsciiCategory 0
  where
    Other = AsciiCategory 0

-- | A punctuation character.
--
-- @since 1.0.0
pattern Punctuation :: AsciiCategory
pattern Punctuation <-
  AsciiCategory 1
  where
    Punctuation = AsciiCategory 1

-- | A letter, either uppercase or lowercase.
--
-- @since 1.0.0
pattern Letter :: AsciiCategory
pattern Letter <-
  AsciiCategory 2
  where
    Letter = AsciiCategory 2

-- | A numerical digit.
--
-- @since 1.0.0
pattern Number :: AsciiCategory
pattern Number <-
  AsciiCategory 3
  where
    Number = AsciiCategory 3

-- | A symbol whose role isn't (normally) punctuation.
--
-- @since 1.0.0
pattern Symbol :: AsciiCategory
pattern Symbol <-
  AsciiCategory 4
  where
    Symbol = AsciiCategory 4

{-# COMPLETE Other, Punctuation, Letter, Number, Symbol #-}

-- | Classify an 'AsciiChar' based on its category.
--
-- >>> categorize [char| ',' |]
-- Punctuation
-- >>> categorize [char| '~' |]
-- Symbol
-- >>> categorize [char| 'w' |]
-- Letter
-- >>> categorize [char| '2' |]
-- Number
-- >>> categorize [char| '\0' |]
-- Other
--
-- @since 1.0.0
categorize :: AsciiChar -> AsciiCategory
categorize c@(AsciiChar w8)
  | charType c == Control = Other
  | w8 == 0x20 = Punctuation
  | w8 >= 0x21 && w8 <= 0x23 = Punctuation
  | w8 == 0x24 = Symbol
  | w8 >= 0x25 && w8 <= 0x2a = Punctuation
  | w8 == 0x2b = Symbol
  | w8 >= 0x2c && w8 <= 0x2f = Punctuation
  | w8 >= 0x30 && w8 <= 0x39 = Number
  | w8 >= 0x3a && w8 <= 0x3b = Punctuation
  | w8 >= 0x3c && w8 <= 0x3e = Symbol
  | w8 >= 0x3f && w8 <= 0x40 = Punctuation
  | w8 >= 0x41 && w8 <= 0x5a = Letter
  | w8 >= 0x5b && w8 <= 0x5d = Punctuation
  | w8 == 0x5e = Symbol
  | w8 == 0x5f = Punctuation
  | w8 == 0x60 = Symbol
  | w8 >= 0x61 && w8 <= 0x7a = Letter
  | w8 == 0x7b = Punctuation
  | w8 == 0x7c = Symbol
  | w8 == 0x7d = Punctuation
  | otherwise = Symbol -- This only leaves ~. - Koz

-- | Compatibility method for the 'GeneralCategory' provided by 'Data.Char'.
--
-- >>> categorizeGeneral [char| ',' |]
-- OtherPunctuation
-- >>> categorizeGeneral [char| '~' |]
-- MathSymbol
-- >>> categorizeGeneral [char| 'w' |]
-- LowercaseLetter
-- >>> categorizeGeneral [char| '2' |]
-- DecimalNumber
-- >>> categorizeGeneral [char| '\0' |]
-- Control
--
-- @since 1.0.0
categorizeGeneral :: AsciiChar -> GeneralCategory
categorizeGeneral (AsciiChar w8) = generalCategory . chr . fromIntegral $ w8

-- | The case of an ASCII character (if it has one).
--
-- @since 1.0.0
newtype AsciiCase = AsciiCase Word8
  deriving (Eq, Ord, Hashable, NFData) via Word8

-- | @since 1.0.0
instance Show AsciiCase where
  {-# INLINEABLE show #-}
  show = \case
    Upper -> "Upper"
    Lower -> "Lower"

-- | @since 1.0.0
instance Bounded AsciiCase where
  minBound = Upper
  maxBound = Lower

-- | Indicator of an uppercase character.
--
-- @since 1.0.0
pattern Upper :: AsciiCase
pattern Upper <-
  AsciiCase 0
  where
    Upper = AsciiCase 0

-- | Indicator of a lowercase character.
--
-- @since 1.0.0
pattern Lower :: AsciiCase
pattern Lower <-
  AsciiCase 1
  where
    Lower = AsciiCase 1

{-# COMPLETE Upper, Lower #-}

-- | Determine the case of an 'AsciiChar'. Returns 'Nothing' if the character
-- doesn't have a case.
--
-- >>> caseOf [char| 'w' |]
-- Just Lower
-- >>> caseOf [char| 'W' |]
-- Just Upper
-- >>> caseOf [char| '~' |]
-- Nothing
--
-- @since 1.0.0
caseOf :: AsciiChar -> Maybe AsciiCase
caseOf c@(AsciiChar w8)
  | categorize c /= Letter = Nothing
  | w8 <= 0x5a = Just Upper
  | otherwise = Just Lower

-- Optics

-- | A representation of the relationship between 'Char' and 'AsciiChar'.
--
-- @since 1.0.0
charWise :: Prism' Char AsciiChar
charWise = prism' (chr . fromIntegral . toByte) fromChar

-- | A representation of the relationship between ASCII characters and bytes.
--
-- @since 1.0.0
byteWise :: Prism' Word8 AsciiChar
byteWise = prism' toByte fromByte
