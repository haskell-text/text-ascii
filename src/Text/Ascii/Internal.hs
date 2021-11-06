{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Text.Ascii.Internal
-- Copyright: (C) 2021 Koz Ross
-- License: Apache 2.0
-- Maintainer: Koz Ross <koz.ross@retro-freedom.nz>
-- Stability: unstable, not subject to PVP
-- Portability: GHC only
--
-- This is an internal module, and is /not/ subject to the PVP. It can change
-- in any way, at any time, and should not be depended on unless you know
-- /exactly/ what you are doing. You have been warned.
module Text.Ascii.Internal where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.CaseInsensitive (FoldCase (foldCase))
import Data.Char (chr, isAscii)
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import qualified Data.List.NonEmpty as NE
import Data.Monoid.Factorial (FactorialMonoid)
import Data.Monoid.GCD (LeftGCDMonoid, RightGCDMonoid)
import Data.Monoid.Monus (OverlappingGCDMonoid)
import Data.Monoid.Null (MonoidNull, PositiveMonoid)
import Data.Semigroup.Cancellative (LeftCancellative, LeftReductive, RightCancellative, RightReductive)
import Data.Semigroup.Factorial (Factorial, StableFactorial)
import Data.Word (Word8)
import GHC.Exts (IsList (Item, fromList, fromListN, toList))
import Numeric (showHex)
import Optics.AffineTraversal (An_AffineTraversal, atraversal)
import Optics.At.Core (Index, IxValue, Ixed (IxKind, ix))
import Text.Megaparsec.Stream
  ( Stream
      ( Token,
        Tokens,
        chunkLength,
        chunkToTokens,
        take1_,
        takeN_,
        takeWhile_,
        tokenToChunk,
        tokensToChunk
      ),
    TraversableStream (reachOffset),
    VisualStream (showTokens),
  )
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
  {-# INLINEABLE minBound #-}
  minBound = AsciiChar 0
  {-# INLINEABLE maxBound #-}
  maxBound = AsciiChar 127

-- | @since 1.0.1
instance FoldCase AsciiChar where
  {-# INLINEABLE foldCase #-}
  foldCase ac@(AsciiChar w8)
    | 65 <= w8 && w8 <= 90 = AsciiChar (w8 + 32)
    | otherwise = ac

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
      Show,
      -- | @since 1.1.1
      Factorial,
      -- | @since 1.1.1
      FactorialMonoid,
      -- | @since 1.1.1
      LeftCancellative,
      -- | @since 1.1.1
      LeftGCDMonoid,
      -- | @since 1.1.1
      LeftReductive,
      -- | @since 1.1.1
      MonoidNull,
      -- | @since 1.1.1
      OverlappingGCDMonoid,
      -- | @since 1.1.1
      PositiveMonoid,
      -- | @since 1.1.1
      RightCancellative,
      -- | @since 1.1.1
      RightGCDMonoid,
      -- | @since 1.1.1
      RightReductive,
      -- | @since 1.1.1
      StableFactorial
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

-- | @since 1.0.1
type instance Index AsciiText = Int

-- | @since 1.0.1
type instance IxValue AsciiText = AsciiChar

-- | @since 1.0.1
instance Ixed AsciiText where
  type IxKind AsciiText = An_AffineTraversal
  {-# INLINEABLE ix #-}
  ix i = atraversal get put
    where
      get :: AsciiText -> Either AsciiText AsciiChar
      get (AsciiText at) = case at BS.!? i of
        Nothing -> Left . AsciiText $ at
        Just w8 -> Right . AsciiChar $ w8
      put :: AsciiText -> AsciiChar -> AsciiText
      put (AsciiText at) (AsciiChar ac) = case BS.splitAt i at of
        (lead, end) -> case BS.uncons end of
          Nothing -> AsciiText at
          Just (_, end') -> AsciiText (lead <> BS.singleton ac <> end')

-- | @since 1.0.1
instance FoldCase AsciiText where
  {-# INLINEABLE foldCase #-}
  foldCase (AsciiText bs) = AsciiText . BS.map go $ bs
    where
      go :: Word8 -> Word8
      go w8
        | 65 <= w8 && w8 <= 90 = w8 + 32
        | otherwise = w8

-- | @since 1.0.1
instance Stream AsciiText where
  type Token AsciiText = AsciiChar
  type Tokens AsciiText = AsciiText
  {-# INLINEABLE tokenToChunk #-}
  tokenToChunk _ = coerce BS.singleton
  {-# INLINEABLE tokensToChunk #-}
  tokensToChunk _ = fromList
  {-# INLINEABLE chunkToTokens #-}
  chunkToTokens _ = toList
  {-# INLINEABLE chunkLength #-}
  chunkLength _ = coerce BS.length
  {-# INLINEABLE take1_ #-}
  take1_ = coerce BS.uncons
  {-# INLINEABLE takeN_ #-}
  takeN_ n at@(AsciiText bs)
    | n <= 0 = Just (coerce BS.empty, at)
    | BS.length bs == 0 = Nothing
    | otherwise = Just . coerce . BS.splitAt n $ bs
  {-# INLINEABLE takeWhile_ #-}
  takeWhile_ = coerce BS.span

-- | @since 1.0.1
instance VisualStream AsciiText where
  {-# INLINEABLE showTokens #-}
  showTokens _ = fmap (chr . fromIntegral) . coerce @_ @[Word8] . NE.toList

-- | @since 1.0.1
instance TraversableStream AsciiText where
  {-# INLINEABLE reachOffset #-}
  reachOffset o ps = coerce (reachOffset o ps)

-- Helpers

isJustAscii :: Word8 -> Maybe Char
isJustAscii w8 =
  if isAscii asChar
    then pure asChar
    else Nothing
  where
    asChar :: Char
    asChar = chr . fromIntegral $ w8
