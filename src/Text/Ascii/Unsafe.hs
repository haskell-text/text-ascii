{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
    head,
    init,
    last,
    maximum,
    minimum,
    scanl1,
    scanr1,
    tail,
  )

newtype Unsafe (a :: Type) = Unsafe {safe :: a}
  deriving
    ( Eq,
      Ord,
      Bounded,
      Hashable,
      NFData,
      FoldCase,
      Semigroup,
      Monoid,
      IsList,
      Stream,
      VisualStream,
      TraversableStream
    )
    via a
  deriving stock (Typeable, Functor)

type role Unsafe nominal

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

head :: Unsafe AsciiText -> AsciiChar
head = coerce BS.head

last :: Unsafe AsciiText -> AsciiChar
last = coerce BS.last

tail :: Unsafe AsciiText -> Unsafe AsciiText
tail = coerce BS.tail

init :: Unsafe AsciiText -> Unsafe AsciiText
init = coerce BS.init

foldl1 :: (AsciiChar -> AsciiChar -> AsciiChar) -> Unsafe AsciiText -> AsciiChar
foldl1 = coerce BS.foldl1

foldl1' :: (AsciiChar -> AsciiChar -> AsciiChar) -> Unsafe AsciiText -> AsciiChar
foldl1' = coerce BS.foldl1'

maximum :: Unsafe AsciiText -> AsciiChar
maximum = coerce BS.maximum

minimum :: Unsafe AsciiText -> AsciiChar
minimum = coerce BS.minimum

scanl1 ::
  (AsciiChar -> AsciiChar -> AsciiChar) ->
  Unsafe AsciiText ->
  Unsafe AsciiText
scanl1 = coerce BS.scanl1

scanr1 ::
  (AsciiChar -> AsciiChar -> AsciiChar) ->
  Unsafe AsciiText ->
  Unsafe AsciiText
scanr1 = coerce BS.scanr1

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
