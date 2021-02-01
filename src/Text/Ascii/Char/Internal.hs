{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Ascii.Char.Internal where

import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.Char (GeneralCategory, chr, generalCategory, isAscii, ord)
import Data.Coerce (coerce)
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.Word (Word8)
import GHC.Exts (IsList (Item, fromList, fromListN, toList))
import Numeric (showHex)
import Optics.Prism (Prism', prism')
import Type.Reflection (Typeable)

-- | Represents valid ASCII characters, which are bytes from @0x00@ to @0x7f@.
--
-- @since 1.0.0
newtype AsciiChar = AsciiChar {toByte :: Word8}
  deriving
    ( -- | @since 1.0.0
      Eq,
      -- | @since 1.0.0
      Ord,
      -- | @since 1.0.0
      Hashable,
      -- | @since 1.0.0
      NFData
    )
    via Word8
  deriving stock
    ( -- | @since 1.0.0
      Typeable
    )

-- | @since 1.0.0
instance Show AsciiChar where
  {-# INLINEABLE show #-}
  show (AsciiChar w8) = "'\\" <> showHex w8 "'"

-- | @since 1.0.0
instance Bounded AsciiChar where
  minBound = AsciiChar 0
  maxBound = AsciiChar 127

-- | View an 'AsciiChar' as its underlying byte. You can pattern match on this,
-- but since there are more bytes than valid ASCII characters, you cannot use
-- this to construct.
--
-- @since 1.0.0
pattern AsByte :: Word8 -> AsciiChar
pattern AsByte w8 <- AsciiChar w8

-- | View an 'AsciiChar' as a 'Char'. You can pattern match on this, but since
-- there are more 'Char's than valid ASCII characters, you cannot use this to
-- construct.
--
-- @since 1.0.0
pattern AsChar :: Char -> AsciiChar
pattern AsChar c <- AsciiChar (isJustAscii -> Just c)

{-# COMPLETE AsByte #-}

{-# COMPLETE AsChar #-}

-- | A string of ASCII characters, represented as a packed byte array.
--
-- @since 1.0.0
newtype AsciiText = AsciiText ByteString
  deriving
    ( -- | @since 1.0.0
      Eq,
      -- | @since 1.0.0
      Ord,
      -- | @since 1.0.0
      NFData,
      -- | @since 1.0.0
      Semigroup,
      -- | @since 1.0.0
      Monoid
    )
    via ByteString
  deriving stock
    ( -- | @since 1.0.0
      Show,
      -- | @since 1.0.0
      Read
    )

-- | @since 1.0.0
instance IsList AsciiText where
  type Item AsciiText = AsciiChar
  {-# INLINEABLE fromList #-}
  fromList =
    coerce @ByteString @AsciiText
      . fromList
      . coerce @[AsciiChar] @[Word8]
  {-# INLINEABLE fromListN #-}
  fromListN n =
    coerce @ByteString @AsciiText
      . fromListN n
      . coerce @[AsciiChar] @[Word8]
  {-# INLINEABLE toList #-}
  toList = coerce . toList . coerce @AsciiText @ByteString

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Text.Ascii.Char.Internal
-- >>> import Text.Ascii.QQ (char)

-- | Try and turn a 'Char' into the equivalent 'AsciiChar'. Will return
-- 'Nothing' if given a 'Char' that has no ASCII equivalent.
--
-- >>> fromChar '0'
-- Just '\0x48'
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
-- Just '\0x4a'
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
-- Just '\0x41'
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
-- Just '\0x63'
-- >>> downcase [char| '\\' |]
-- Nothing
--
-- @since 1.0.0
downcase :: AsciiChar -> Maybe AsciiChar
downcase c@(AsciiChar w8) =
  caseOf c >>= (\cs -> guard (cs == Upper) $> AsciiChar (w8 + 32))

-- Categorization

newtype AsciiType = AsciiType Word8
  deriving (Eq, Ord, Hashable, NFData) via Word8
  deriving stock (Show, Read)

instance Bounded AsciiType where
  minBound = Control
  maxBound = Printable

pattern Control :: AsciiType
pattern Control <-
  AsciiType 0
  where
    Control = AsciiType 0

pattern Printable :: AsciiType
pattern Printable <-
  AsciiType 1
  where
    Printable = AsciiType 1

{-# COMPLETE Control, Printable #-}

charType :: AsciiChar -> AsciiType
charType (AsciiChar w8)
  | w8 == 127 = Control
  | w8 < 32 = Control
  | otherwise = Printable

newtype AsciiCategory = AsciiCategory Word8
  deriving (Eq, Ord, Hashable, NFData) via Word8
  deriving stock (Show, Read)

instance Bounded AsciiCategory where
  minBound = Other
  maxBound = Symbol

pattern Other :: AsciiCategory
pattern Other <-
  AsciiCategory 0
  where
    Other = AsciiCategory 0

pattern Punctuation :: AsciiCategory
pattern Punctuation <-
  AsciiCategory 1
  where
    Punctuation = AsciiCategory 1

pattern Letter :: AsciiCategory
pattern Letter <-
  AsciiCategory 2
  where
    Letter = AsciiCategory 2

pattern Number :: AsciiCategory
pattern Number <-
  AsciiCategory 3
  where
    Number = AsciiCategory 3

pattern Symbol :: AsciiCategory
pattern Symbol <-
  AsciiCategory 4
  where
    Symbol = AsciiCategory 4

{-# COMPLETE Other, Punctuation, Letter, Number, Symbol #-}

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

categorizeGeneral :: AsciiChar -> GeneralCategory
categorizeGeneral (AsciiChar w8) = generalCategory . chr . fromIntegral $ w8

newtype AsciiCase = AsciiCase Word8
  deriving (Eq, Ord, Hashable, NFData) via Word8
  deriving stock (Show, Read)

instance Bounded AsciiCase where
  minBound = Upper
  maxBound = Lower

pattern Upper :: AsciiCase
pattern Upper <-
  AsciiCase 0
  where
    Upper = AsciiCase 0

pattern Lower :: AsciiCase
pattern Lower <-
  AsciiCase 1
  where
    Lower = AsciiCase 1

{-# COMPLETE Upper, Lower #-}

caseOf :: AsciiChar -> Maybe AsciiCase
caseOf c@(AsciiChar w8)
  | categorize c /= Letter = Nothing
  | w8 <= 0x51 = Just Upper
  | otherwise = Just Lower

-- Optics

charWise :: Prism' Char AsciiChar
charWise = prism' (chr . fromIntegral . toByte) fromChar

byteWise :: Prism' Word8 AsciiChar
byteWise = prism' toByte fromByte

-- Helpers

isJustAscii :: Word8 -> Maybe Char
isJustAscii w8 =
  if isAscii asChar
    then pure asChar
    else Nothing
  where
    asChar :: Char
    asChar = chr . fromIntegral $ w8
