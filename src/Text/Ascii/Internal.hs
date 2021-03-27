{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM_, when)
import Control.Monad.ST (ST, runST)
import Data.CaseInsensitive (FoldCase (foldCase))
import Data.Char (chr, isAscii)
import Data.Coerce (coerce)
import Data.Foldable (foldl', traverse_)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Primitive.ByteArray
  ( ByteArray,
    MutableByteArray,
    byteArrayFromListN,
    compareByteArrays,
    copyByteArray,
    indexByteArray,
    newByteArray,
    readByteArray,
    unsafeFreezeByteArray,
    writeByteArray,
  )
import Data.Semigroup (sconcat, stimes)
import Data.Word (Word8)
import GHC.Exts (IsList (Item, fromList, fromListN, toList))
import Numeric (showHex)
import Optics.AffineTraversal (An_AffineTraversal, atraversal)
import Optics.At.Core (Index, IxValue, Ixed (IxKind, ix))
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
data AsciiText
  = AT {-# UNPACK #-} !ByteArray {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- Backing store, offset index, length
-- Maintains the following invariants:
-- 1) offset, length > 0
-- 2) offset + length <= ba.length

-- | @since 1.0.0
instance Eq AsciiText where
  {-# INLINEABLE (==) #-}
  AT ba off len == AT ba' off' len'
    | len /= len' = False
    | otherwise = case compareByteArrays ba off ba' off' len of
      EQ -> True
      _ -> False

-- | @since 1.0.0
instance Ord AsciiText where
  {-# INLINEABLE compare #-}
  compare (AT ba off len) (AT ba' off' len') =
    compareByteArrays ba off ba' off' (min len len') <> compare len len'

-- | @since 1.0.0
instance NFData AsciiText where
  {-# INLINEABLE rnf #-}
  rnf (AT _ off len) = off `seq` len `seq` ()

-- | @since 1.0.0
instance Show AsciiText where
  {-# INLINEABLE show #-}
  show (AT ba off len) =
    ['"'] <> fmap go [off .. off + len - 1] <> ['"']
    where
      go :: Int -> Char
      go i = chr . fromIntegral . indexByteArray @Word8 ba $ i

-- | @since 1.0.0
instance Semigroup AsciiText where
  {-# INLINEABLE (<>) #-}
  AT ba off len <> AT ba' off' len' = runST $ do
    mba <- newByteArray (len + len')
    copyByteArray mba 0 ba off len
    copyByteArray mba len ba' off' len'
    frozen <- unsafeFreezeByteArray mba
    pure . AT frozen 0 $ len + len'
  {-# INLINEABLE sconcat #-}
  sconcat ne =
    let totalLen = foldl' go 0 ne
     in runST $ do
          mba <- newByteArray totalLen
          foldM_ (go2 mba) 0 ne
          frozen <- unsafeFreezeByteArray mba
          pure . AT frozen 0 $ totalLen
    where
      go :: Int -> AsciiText -> Int
      go acc (AT _ _ len) = acc + len
      go2 :: MutableByteArray s -> Int -> AsciiText -> ST s Int
      go2 mba pos (AT ba off len) = do
        copyByteArray mba pos ba off len
        pure (pos + len)
  {-# INLINEABLE stimes #-}
  stimes n (AT ba off len) = runST $ do
    let n' = fromIntegral n
    let totalLen = n' * len
    mba <- newByteArray totalLen
    go mba n' 0
    frozen <- unsafeFreezeByteArray mba
    pure . AT frozen 0 $ totalLen
    where
      go :: MutableByteArray s -> Int -> Int -> ST s ()
      go mba lim pos
        | pos == lim = pure ()
        | otherwise = do
          copyByteArray mba pos ba off len
          go mba lim (pos + len)

-- | @since 1.0.0
instance Monoid AsciiText where
  {-# INLINEABLE mempty #-}
  mempty = AT (byteArrayFromListN @Word8 0 mempty) 0 0
  {-# INLINEABLE mconcat #-}
  mconcat = \case
    [] -> mempty
    (t : ts) -> sconcat (t :| ts)

-- | @since 1.0.0
instance IsList AsciiText where
  type Item AsciiText = AsciiChar
  {-# INLINEABLE fromList #-}
  fromList ell =
    let len = length ell
     in AT (byteArrayFromListN len . coerce @_ @[Word8] $ ell) 0 len
  {-# INLINEABLE fromListN #-}
  fromListN len ell =
    AT (byteArrayFromListN len . coerce @_ @[Word8] $ ell) 0 len
  {-# INLINEABLE toList #-}
  toList (AT ba off len) = fmap go [off .. off + len - 1]
    where
      go :: Int -> AsciiChar
      go = coerce @Word8 @AsciiChar . indexByteArray ba

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
      get t@(AT ba off len)
        | i >= 0 && i < len =
          pure . AsciiChar . indexByteArray ba $ off + i
        | otherwise = Left t
      put :: AsciiText -> AsciiChar -> AsciiText
      put t@(AT ba off len) (AsciiChar w8)
        | i >= 0 && i < len = runST $ do
          mba <- newByteArray len
          copyByteArray mba 0 ba off len
          writeByteArray mba i w8
          frozen <- unsafeFreezeByteArray mba
          pure . AT frozen 0 $ len
        | otherwise = t

-- | @since 1.0.1
instance FoldCase AsciiText where
  {-# INLINEABLE foldCase #-}
  foldCase (AT ba off len) = runST $ do
    mba <- newByteArray len
    copyByteArray mba 0 ba off len
    traverse_ (go mba) [0 .. len - 1]
    frozen <- unsafeFreezeByteArray mba
    pure . AT frozen 0 $ len
    where
      go :: MutableByteArray s -> Int -> ST s ()
      go mba i = do
        w8 <- readByteArray @Word8 mba i
        when (65 <= w8 && w8 <= 90) (writeByteArray mba i (w8 + 32))

-- | @since 2.0.0
instance Arbitrary AsciiText where
  {-# INLINEABLE arbitrary #-}
  arbitrary = _
  {-# INLINEABLE shrink #-}
  shrink = _

{-
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
-}

-- Helpers

isJustAscii :: Word8 -> Maybe Char
isJustAscii w8 =
  if isAscii asChar
    then pure asChar
    else Nothing
  where
    asChar :: Char
    asChar = chr . fromIntegral $ w8
