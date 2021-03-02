{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
  )
where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.CaseInsensitive (FoldCase)
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Word (Word8)
import GHC.Exts (IsList)
import GHC.Read (expectP, lexP, parens, readPrec)
import Text.Ascii.Internal (AsciiChar (AsciiChar), AsciiText (AsciiText))
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
      Show
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

-- | @since 1.0.1
instance Read (Unsafe AsciiText) where
  {-# INLINEABLE readPrec #-}
  readPrec = Unsafe . AsciiText <$> go
    where
      go :: ReadPrec ByteString
      go = do
        bs :: ByteString <- readPrec
        case BS.findIndex (>= 128) bs of
          Nothing -> pure bs
          Just i -> error $ "Non-ASCII byte at index " <> show i

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
head = coerce BS.head

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
last = coerce BS.last

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
tail = coerce BS.tail

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
init = coerce BS.init

-- | Left-associative fold of a text without a base case.
--
-- /Requirements:/ Text is not empty.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
foldl1 :: (AsciiChar -> AsciiChar -> AsciiChar) -> Unsafe AsciiText -> AsciiChar
foldl1 = coerce BS.foldl1

-- | Left-associative fold of a text without a base case, strict in the
-- accumulator.
--
-- /Requirements:/ Text is not empty.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
foldl1' :: (AsciiChar -> AsciiChar -> AsciiChar) -> Unsafe AsciiText -> AsciiChar
foldl1' = coerce BS.foldl1'

-- | Right-associative fold of a text without a base case.
--
-- /Requirements:/ Text is not empty.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
foldr1 :: (AsciiChar -> AsciiChar -> AsciiChar) -> Unsafe AsciiText -> AsciiChar
foldr1 = coerce BS.foldr1

-- | Right-associative fold of a text without a base case, strict in the
-- accumulator.
--
-- /Requirements:/ Text is not empty.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
foldr1' :: (AsciiChar -> AsciiChar -> AsciiChar) -> Unsafe AsciiText -> AsciiChar
foldr1' = coerce BS.foldr1'

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
maximum = coerce BS.maximum

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
minimum = coerce BS.minimum

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
scanl1 = coerce BS.scanl1

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
scanr1 = coerce BS.scanr1

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
index = coerce BS.index

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
