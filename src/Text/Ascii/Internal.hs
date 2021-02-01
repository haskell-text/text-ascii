{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Ascii.Internal where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Char (chr, isAscii)
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.Word (Word8)
import GHC.Exts (IsList (Item, fromList, fromListN, toList))
import Numeric (showHex)
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
  show (AsciiChar w8) = "'0x" <> showHex w8 "'"

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
      Monoid,
      -- | @since 1.0.0
      Show
    )
    via ByteString

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

-- Helpers

isJustAscii :: Word8 -> Maybe Char
isJustAscii w8 =
  if isAscii asChar
    then pure asChar
    else Nothing
  where
    asChar :: Char
    asChar = chr . fromIntegral $ w8
