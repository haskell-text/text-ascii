{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module: Text.Ascii.Unsafe
-- Copyright: (C) 2021 Koz Ross
-- License: Apache 2.0
-- Maintainer: Koz Ross <koz.ross@retro-freedom.nz>
-- Stability: stable
-- Portability: GHC only
--
-- A wrapper for partial type class instances and functions.
--
-- This module is designed for qualified importing:
--
-- > import qualified Text.Ascii.Unsafe as Unsafe
--
-- = Note
--
-- The functions in this module still perform bounds checks and ensure
-- invariants are maintained; thus, they're not any faster than their total
-- cousins. The goals of this module are to allow the use of partial functions
-- in contexts where you know the requirements hold, but can't prove it to the
-- compiler.
module Text.Ascii.Unsafe
  ( -- * Types
    Unsafe (..),

    -- * Text functions
    head,
    last,
    tail,
    init,
    foldl1,
    foldl1',
    foldr1,
    foldr1',
    maximum,
    minimum,
    scanl1,
    scanr1,
    index,

    -- * Decoding
    decodeAscii,
    decodeBytesAscii,
  )
where

import Control.DeepSeq (NFData)
import Data.CaseInsensitive (FoldCase)
import Data.Char (isAscii, ord)
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import Data.Hashable (Hashable)
import Data.Kind (Type)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Primitive.ByteArray (indexByteArray)
import Data.Primitive.Types (Prim)
import Data.Word (Word8)
import GHC.Exts (IsList (Item, fromList, toList))
import GHC.Read (expectP, lexP, parens, readPrec)
import System.Random.Stateful (Uniform (uniformM), UniformRange (uniformRM))
import Test.QuickCheck.Arbitrary (Arbitrary)
import Text.Ascii.Internal (AsciiChar (AsciiChar), AsciiText (AT))
import Text.Megaparsec.Stream (Stream, TraversableStream, VisualStream)
import Text.ParserCombinators.ReadPrec (ReadPrec)
import Text.Read (Lexeme (Char))
import Type.Reflection (Typeable)
import Prelude hiding
  ( foldl1,
    foldr1,
    head,
    init,
    last,
    maximum,
    minimum,
    scanl1,
    scanr1,
    tail,
  )

-- | A wrapper for a type, designating that partial type class methods or other
-- functions are available for it.
--
-- The role of 'Unsafe''s type argument is set to nominal. Among other things,
-- it means that this type can't be coerced or derived through. This ensures
-- clear indication when (and to what extent) non-total operations occur in any
-- code using them.
--
-- @since 1.0.1
newtype Unsafe (a :: Type) = Unsafe {safe :: a}
  deriving
    ( -- | @since 1.0.1
      Eq,
      -- | @since 1.0.1
      Ord,
      -- | @since 1.0.1
      Bounded,
      -- | @since 1.0.1
      Hashable,
      -- | @since 1.0.1
      NFData,
      -- | @since 1.0.1
      FoldCase,
      -- | @since 1.0.1
      Semigroup,
      -- | @since 1.0.1
      Monoid,
      -- | @since 1.0.1
      IsList,
      -- | @since 1.0.1
      Stream,
      -- | @since 1.0.1
      VisualStream,
      -- | @since 1.0.1
      TraversableStream,
      -- | @since 1.0.1
      Show,
      -- | @since 2.0.0
      Arbitrary,
      -- | @since 2.0.0
      Prim
    )
    via a
  deriving stock
    ( -- | @since 1.0.1
      Typeable,
      -- | @since 1.0.1
      Functor
    )

type role Unsafe nominal

-- | @since 1.0.1
instance Read (Unsafe AsciiChar) where
  {-# INLINEABLE readPrec #-}
  readPrec = parens go
    where
      go :: ReadPrec (Unsafe AsciiChar)
      go =
        Unsafe . AsciiChar <$> do
          expectP (Char '\'')
          expectP (Char '0')
          expectP (Char 'x')
          Char d1 <- lexP
          Char d2 <- lexP
          expectP (Char '\'')
          case d1 of
            '0' -> fromSecondDigit d2
            '1' -> (16 +) <$> fromSecondDigit d2
            '2' -> (32 +) <$> fromSecondDigit d2
            '3' -> (48 +) <$> fromSecondDigit d2
            '4' -> (64 +) <$> fromSecondDigit d2
            '5' -> (80 +) <$> fromSecondDigit d2
            '6' -> (96 +) <$> fromSecondDigit d2
            '7' -> (112 +) <$> fromSecondDigit d2
            _ -> fail $ "Expected digit from 0 to 7, instead got '" <> [d1] <> "'"

-- | @since 1.0.1
instance Enum (Unsafe AsciiChar) where
  {-# INLINEABLE succ #-}
  succ (Unsafe (AsciiChar w8))
    | w8 < 127 = Unsafe . AsciiChar $ w8 + 1
    | otherwise = error "Out of range for ASCII character"
  {-# INLINEABLE pred #-}
  pred (Unsafe (AsciiChar w8))
    | w8 > 0 = Unsafe . AsciiChar $ w8 - 1
    | otherwise = error "Out of range for ASCII character"
  {-# INLINEABLE toEnum #-}
  toEnum n
    | 0 <= n && n <= 127 = Unsafe . AsciiChar . fromIntegral $ n
    | otherwise = error "Out of range for ASCII character"
  {-# INLINEABLE fromEnum #-}
  fromEnum (Unsafe (AsciiChar w8)) = fromIntegral w8
  {-# INLINEABLE enumFrom #-}
  enumFrom (Unsafe (AsciiChar w8)) = coerce [w | w <- [w8 ..], w <= 127]
  {-# INLINEABLE enumFromThen #-}
  enumFromThen (Unsafe (AsciiChar start)) (Unsafe (AsciiChar step)) =
    coerce [w | w <- [start, step ..], w <= 127]
  {-# INLINEABLE enumFromTo #-}
  enumFromTo (Unsafe (AsciiChar start)) (Unsafe (AsciiChar end)) =
    coerce [w | w <- [start .. end], w <= 127]
  {-# INLINEABLE enumFromThenTo #-}
  enumFromThenTo (Unsafe (AsciiChar start)) (Unsafe (AsciiChar step)) (Unsafe (AsciiChar end)) =
    coerce [w | w <- [start, step .. end], w <= 127]

-- | @since 2.0.0
instance (Uniform a) => Uniform (Unsafe a) where
  {-# INLINEABLE uniformM #-}
  uniformM gen = Unsafe <$> uniformM gen

-- | @since 2.0.0
instance (UniformRange a) => UniformRange (Unsafe a) where
  {-# INLINEABLE uniformRM #-}
  uniformRM (Unsafe lo, Unsafe hi) gen = Unsafe <$> uniformRM (lo, hi) gen

-- | @since 1.0.1
instance Read (Unsafe AsciiText) where
  {-# INLINEABLE readPrec #-}
  readPrec = do
    strink :: String <- readPrec
    asciiChars <- traverse go strink
    pure . Unsafe . fromList $ asciiChars
    where
      go :: Char -> ReadPrec AsciiChar
      go c
        | isAscii c = pure . AsciiChar . fromIntegral . ord $ c
        | otherwise = fail $ "Read: Not an ASCII character: " <> [c]

-- Functions

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XQuasiQuotes
-- >>> import Text.Ascii.Unsafe
-- >>> import Text.Ascii (ascii)
-- >>> import Prelude ((.), ($))

-- | Yield the first character of the text.
--
-- /Requirements:/ Text is not empty.
--
-- >>> head . Unsafe $ [ascii| "catboy" |]
-- '0x63'
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.1
head :: Unsafe AsciiText -> AsciiChar
head (Unsafe (AT ba off len))
  | len == 0 =
    error "Tried to take the head of an empty text."
  | otherwise = AsciiChar . indexByteArray ba $ off

-- | Yield the last character of the text.
--
-- /Requirements:/ Text is not empty.
--
-- >>> last . Unsafe $ [ascii| "catboy" |]
-- '0x79'
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.1
last :: Unsafe AsciiText -> AsciiChar
last (Unsafe (AT ba off len))
  | len == 0 =
    error "Tried to take the last of an empty text."
  | otherwise = AsciiChar . indexByteArray ba $ off + len - 1

-- | Yield the text without its first character.
--
-- /Requirements:/ Text is not empty.
--
-- >>> tail . Unsafe $ [ascii| "catboy" |]
-- "atboy"
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.1
tail :: Unsafe AsciiText -> Unsafe AsciiText
tail (Unsafe (AT ba off len))
  | len == 0 =
    error "Tried to take the tail of an empty text."
  | otherwise = Unsafe . AT ba (off + 1) $ len - 1

-- | Yield the text without its last character.
--
-- /Requirements:/ Text is not empty.
--
-- >>> init . Unsafe $ [ascii| "catboy" |]
-- "catbo"
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.1
init :: Unsafe AsciiText -> Unsafe AsciiText
init (Unsafe (AT ba off len))
  | len == 0 =
    error "Tried to take the init of an empty text."
  | otherwise = Unsafe . AT ba off $ len - 1

-- | Left-associative fold of a text without a base case.
--
-- /Requirements:/ Text is not empty.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
foldl1 :: (AsciiChar -> AsciiChar -> AsciiChar) -> Unsafe AsciiText -> AsciiChar
foldl1 f (Unsafe at) = F.foldl1 f . toList $ at

-- | Left-associative fold of a text without a base case, strict in the
-- accumulator.
--
-- /Requirements:/ Text is not empty.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
foldl1' :: (AsciiChar -> AsciiChar -> AsciiChar) -> Unsafe AsciiText -> AsciiChar
foldl1' f (Unsafe at) = L.foldl1' f . toList $ at

-- | Right-associative fold of a text without a base case.
--
-- /Requirements:/ Text is not empty.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
foldr1 :: (AsciiChar -> AsciiChar -> AsciiChar) -> Unsafe AsciiText -> AsciiChar
foldr1 f (Unsafe at) = F.foldr1 f . toList $ at

-- | Right-associative fold of a text without a base case, strict in the
-- accumulator.
--
-- /Requirements:/ Text is not empty.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
foldr1' :: (AsciiChar -> AsciiChar -> AsciiChar) -> Unsafe AsciiText -> AsciiChar
foldr1' f (Unsafe at) = go . toList $ at
  where
    go :: [AsciiChar] -> AsciiChar
    go = \case
      [x] -> x
      (x : xs) -> x `seq` f x (go xs)
      [] -> error "Attempted to foldr1' an empty text"

-- | Yield the character in the text whose byte representation is numerically
-- the largest.
--
-- /Requirements:/ Text is not empty.
--
-- >>> maximum . Unsafe $ [ascii| "catboy" |]
-- '0x79'
-- >>> maximum . Unsafe $ [ascii| "nyan~" |]
-- '0x7e'
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
maximum :: Unsafe AsciiText -> AsciiChar
maximum (Unsafe at) = F.maximum . toList $ at

-- | Yield the character in the text whose byte representation is numerically
-- the smallest.
--
-- /Requirements:/ Text is not empty.
--
-- >>> minimum . Unsafe $ [ascii| "catboy" |]
-- '0x61'
-- >>> minimum . Unsafe $ [ascii| " nyan" |]
-- '0x20'
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
minimum :: Unsafe AsciiText -> AsciiChar
minimum (Unsafe at) = F.minimum . toList $ at

-- | 'scanl1' is similar to 'foldl1', but returns a list of successive values
-- from the left.
--
-- /Requirements:/ Text is not empty.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
scanl1 ::
  -- | accumulator -> element -> new accumulator
  (AsciiChar -> AsciiChar -> AsciiChar) ->
  -- | Input of length \(n\)
  Unsafe AsciiText ->
  -- | Output of length \(n - 1\)
  Unsafe AsciiText
scanl1 f (Unsafe at) = Unsafe . fromList . L.scanl1 f . toList $ at

-- | 'scanr1' is similar to 'foldr1', but returns a list of successive values
-- from the right.
--
-- /Requirements:/ Text is not empty.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
scanr1 ::
  -- | element -> accumulator -> new accumulator
  (AsciiChar -> AsciiChar -> AsciiChar) ->
  -- | Input of length \(n\)
  Unsafe AsciiText ->
  -- | Output of length \(n - 1\)
  Unsafe AsciiText
scanr1 f (Unsafe at) = Unsafe . fromList . L.scanr1 f . toList $ at

-- | Yield the character at the given position.
--
-- /Requirements:/ The position must be at least 0, and at most the length of
-- the text - 1.
--
-- >>> index (Unsafe [ascii| "catboy" |]) 0
-- '0x63'
-- >>> index (Unsafe $ [ascii| "catboy" |]) 4
-- '0x6f'
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.1
index :: Unsafe AsciiText -> Int -> AsciiChar
index (Unsafe (AT ba off len)) i
  | i < 0 = error $ "Attempted to index text at negative position: " <> show i
  | i >= len = error $ "Attempted to index text outside of bounds: " <> show i
  | otherwise = AsciiChar . indexByteArray ba $ off + i

-- | Unsafely decodes any string-like type into an ASCII text.
--
-- /Requirements:/ Every element of the input must be representable as ASCII.
--
-- >>> decodeAscii ("I am a catboy" :: String)
-- "I am a catboy"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 2.0.0
decodeAscii :: (IsList s, Item s ~ Char) => s -> Unsafe AsciiText
decodeAscii = Unsafe . fromList . fromMaybe err . traverse go . toList
  where
    err :: [AsciiChar]
    err = error "Failed to decode string into ASCII"
    go :: Char -> Maybe AsciiChar
    go c
      | isAscii c = pure . AsciiChar . fromIntegral . ord $ c
      | otherwise = Nothing

-- | Unsafely decodes any bytestring-like type into an ASCII text.
--
-- /Requirements:/ Every byte of the input must be between 0x00 and 0x7F
-- inclusive.
--
-- >>> decodeAsciiBytes ([0x6e, 0x79, 0x61, 0x6e] :: [Word8])
-- "nyan"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 2.0.0
decodeBytesAscii :: (IsList s, Item s ~ Word8) => s -> Unsafe AsciiText
decodeBytesAscii = Unsafe . fromList . fromMaybe err . traverse go . toList
  where
    err :: [AsciiChar]
    err = error "Failed decode bytes into ASCII"
    go :: Word8 -> Maybe AsciiChar
    go w8
      | w8 <= 127 = Just . AsciiChar $ w8
      | otherwise = Nothing

-- Helpers

fromSecondDigit :: Char -> ReadPrec Word8
fromSecondDigit = \case
  '0' -> pure 0
  '1' -> pure 1
  '2' -> pure 2
  '3' -> pure 3
  '4' -> pure 4
  '5' -> pure 5
  '6' -> pure 6
  '7' -> pure 7
  '8' -> pure 8
  '9' -> pure 9
  'a' -> pure 10
  'b' -> pure 11
  'c' -> pure 12
  'd' -> pure 13
  'e' -> pure 14
  'f' -> pure 15
  d -> fail $ "Expected hex digit, instead got '" <> [d] <> "'"
