{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
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
import Data.Bifunctor (bimap)
import Data.CaseInsensitive (FoldCase (foldCase))
import Data.Char (chr, isAscii)
import Data.Coerce (coerce)
import Data.Foldable (foldl', traverse_)
import Data.Hashable (Hashable (hashWithSalt))
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
import Data.Primitive.Types (Prim)
import Data.Semigroup (sconcat, stimes)
import Data.Word (Word8)
import GHC.Exts (IsList (Item, fromList, fromListN, toList))
import Numeric (showHex)
import Optics.AffineTraversal (An_AffineTraversal, atraversal)
import Optics.At.Core (Index, IxValue, Ixed (IxKind, ix))
import System.Random.Stateful (Uniform (uniformM), UniformRange (uniformRM))
import Test.QuickCheck.Arbitrary
  ( Arbitrary (arbitrary, shrink),
    liftArbitrary,
    liftShrink,
  )
import Test.QuickCheck.Gen (chooseBoundedIntegral)
import Text.Megaparsec
  ( Pos,
    PosState (PosState),
    SourcePos (SourcePos),
    mkPos,
    pos1,
    pstateInput,
    pstateLinePrefix,
    pstateOffset,
    pstateSourcePos,
    pstateTabWidth,
    sourceLine,
    unPos,
  )
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
      NFData,
      -- | @since 2.0.0
      Prim
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

-- | @since 2.0.0
instance Arbitrary AsciiChar where
  {-# INLINEABLE arbitrary #-}
  arbitrary = AsciiChar <$> chooseBoundedIntegral (0, 127)

-- | @since 2.0.0
instance Uniform AsciiChar where
  {-# INLINEABLE uniformM #-}
  uniformM gen = AsciiChar <$> uniformRM (0, 127) gen

-- | @since 2.0.0
instance UniformRange AsciiChar where
  {-# INLINEABLE uniformRM #-}
  uniformRM (AsciiChar w8, AsciiChar w8') gen =
    AsciiChar <$> uniformRM (w8, w8') gen

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
  show (AT ba off len) = show . fmap go $ [off .. off + len - 1]
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
    go mba totalLen 0
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
  arbitrary = fromList <$> liftArbitrary arbitrary
  {-# INLINEABLE shrink #-}
  shrink = fmap fromList . liftShrink shrink . toList

-- | @since 2.0.0
instance Hashable AsciiText where
  {-# INLINEABLE hashWithSalt #-}
  hashWithSalt salt = hashWithSalt salt . toList

-- | @since 1.0.1
instance Stream AsciiText where
  type Token AsciiText = AsciiChar
  type Tokens AsciiText = AsciiText
  {-# INLINEABLE tokenToChunk #-}
  tokenToChunk _ (AsciiChar w8) = AT (fromListN 1 [w8]) 0 1
  {-# INLINEABLE chunkToTokens #-}
  chunkToTokens _ = toList
  {-# INLINEABLE tokensToChunk #-}
  tokensToChunk _ = fromList
  {-# INLINEABLE chunkLength #-}
  chunkLength _ (AT _ _ len) = len
  {-# INLINEABLE take1_ #-}
  take1_ (AT ba off len)
    | len == 0 = Nothing
    | otherwise =
      Just (AsciiChar . indexByteArray ba $ off, AT ba (off + 1) $ len - 1)
  {-# INLINEABLE takeN_ #-}
  takeN_ n at@(AT ba off len)
    | n <= 0 = Just (mempty, at)
    | len == 0 = Nothing
    | n < len = Just (AT ba off len, AT ba (off + n) (len - n))
    | otherwise = Just (at, mempty)
  {-# INLINEABLE takeWhile_ #-}
  takeWhile_ f = bimap fromList fromList . span f . toList

-- | @since 1.0.1
instance VisualStream AsciiText where
  {-# INLINEABLE showTokens #-}
  showTokens _ = fmap (chr . fromIntegral) . coerce @_ @[Word8] . toList

-- | @since 1.0.1
instance TraversableStream AsciiText where
  {-# INLINEABLE reachOffset #-}
  reachOffset =
    reachOffset' splitAt' foldl'' toString' toChar' (AsciiChar 0x0a, AsciiChar 0x09)
    where
      splitAt' :: Int -> AsciiText -> (AsciiText, AsciiText)
      splitAt' i at@(AT ba off len)
        | i <= 0 = (mempty, at)
        | i < len = (AT ba off i, AT ba (off + i) (len - i))
        | otherwise = (at, mempty)
      foldl'' :: forall b. (b -> AsciiChar -> b) -> b -> AsciiText -> b
      foldl'' f x = foldl' f x . toList
      toString' :: AsciiText -> String
      toString' = fmap toChar' . toList
      toChar' :: AsciiChar -> Char
      toChar' (AsChar c) = c

-- Helpers

-- Internal helper state combining a difference string and an unboxed source
-- position. Borrowed from megaparsec.
data St = St SourcePos ShowS

-- A helper definition to facilitate defining 'reachOffset' for various
-- stream types. Borrowed from megaparsec.
reachOffset' ::
  forall s.
  Stream s =>
  -- | How to split input stream at given offset
  (Int -> s -> (Tokens s, s)) ->
  -- | How to fold over input stream
  (forall b. (b -> Token s -> b) -> b -> Tokens s -> b) ->
  -- | How to convert chunk of input stream into a 'String'
  (Tokens s -> String) ->
  -- | How to convert a token into a 'Char'
  (Token s -> Char) ->
  -- | Newline token and tab token
  (Token s, Token s) ->
  -- | Offset to reach
  Int ->
  -- | Initial 'PosState' to use
  PosState s ->
  -- | Line at which 'SourcePos' is located, updated 'PosState'
  (Maybe String, PosState s)
reachOffset'
  splitAt'
  foldl''
  fromToks
  fromTok
  (newlineTok, tabTok)
  o
  PosState {..} =
    ( Just $ case expandTab pstateTabWidth
        . addPrefix
        . f
        . fromToks
        . fst
        $ takeWhile_ (/= newlineTok) post of
        "" -> "<empty line>"
        xs -> xs,
      PosState
        { pstateInput = post,
          pstateOffset = max pstateOffset o,
          pstateSourcePos = spos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix =
            if sameLine
              then -- NOTE We don't use difference lists here because it's
              -- desirable for 'PosState' to be an instance of 'Eq' and
              -- 'Show'. So we just do appending here. Fortunately several
              -- parse errors on the same line should be relatively rare.
                pstateLinePrefix ++ f ""
              else f ""
        }
    )
    where
      addPrefix xs =
        if sameLine
          then pstateLinePrefix ++ xs
          else xs
      sameLine = sourceLine spos == sourceLine pstateSourcePos
      (pre, post) = splitAt' (o - pstateOffset) pstateInput
      St spos f = foldl'' go (St pstateSourcePos id) pre
      go (St apos g) ch =
        let SourcePos n l c = apos
            c' = unPos c
            w = unPos pstateTabWidth
         in if
                | ch == newlineTok ->
                  St
                    (SourcePos n (l <> pos1) pos1)
                    id
                | ch == tabTok ->
                  St
                    (SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w)))
                    (g . (fromTok ch :))
                | otherwise ->
                  St
                    (SourcePos n l (c <> pos1))
                    (g . (fromTok ch :))
{-# INLINE reachOffset' #-}

-- Replace tab characters with given number of spaces. Borrowed from megaparsec.
expandTab ::
  Pos ->
  String ->
  String
expandTab w' = go 0
  where
    go 0 [] = []
    go 0 ('\t' : xs) = go w xs
    go 0 (x : xs) = x : go 0 xs
    go n xs = ' ' : go (n - 1) xs
    w = unPos w'

isJustAscii :: Word8 -> Maybe Char
isJustAscii w8 =
  if isAscii asChar
    then pure asChar
    else Nothing
  where
    asChar :: Char
    asChar = chr . fromIntegral $ w8
