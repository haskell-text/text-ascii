{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Ascii.Char.Internal where

import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Char (GeneralCategory, chr, generalCategory, isAscii, ord)
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.Word (Word8)
import Optics.Prism (Prism', prism')
import Type.Reflection (Typeable)

newtype AsciiChar = AsciiChar {toByte :: Word8}
  deriving (Eq, Ord, Hashable, NFData) via Word8
  deriving stock (Show, Typeable)

instance Bounded AsciiChar where
  minBound = AsciiChar 0
  maxBound = AsciiChar 127

pattern AsByte :: Word8 -> AsciiChar
pattern AsByte w8 <- AsciiChar w8

pattern AsChar :: Char -> AsciiChar
pattern AsChar c <- AsciiChar (isJustAscii -> Just c)

{-# COMPLETE AsByte #-}

{-# COMPLETE AsChar #-}

fromChar :: Char -> Maybe AsciiChar
fromChar c =
  if isAscii c
    then pure . AsciiChar . fromIntegral . ord $ c
    else Nothing

fromByte :: Word8 -> Maybe AsciiChar
fromByte w8 =
  if isAscii . chr . fromIntegral $ w8
    then pure . AsciiChar $ w8
    else Nothing

upcase :: AsciiChar -> Maybe AsciiChar
upcase c@(AsciiChar w8) = caseOf c >>= (\cs -> guard (cs == Lower) $> AsciiChar (w8 - 32))

downcase :: AsciiChar -> Maybe AsciiChar
downcase c@(AsciiChar w8) = caseOf c >>= (\cs -> guard (cs == Upper) $> AsciiChar (w8 + 32))

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
