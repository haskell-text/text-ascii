{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Text.Ascii
-- Copyright: (C) 2021 Koz Ross
-- License: Apache 2.0
-- Maintainer: Koz Ross <koz.ross@retro-freedom.nz>
-- Stability: stable
-- Portability: GHC only
--
-- An implementation of ASCII strings.
--
-- This module is designed for qualified importing:
--
-- > import qualified Text.Ascii as Ascii
--
-- /See also:/ [Wikipedia entry for ASCII](https://en.wikipedia.org/wiki/ASCII)
module Text.Ascii
  ( -- * Type
    AsciiText,

    -- * Creation
    empty,
    singleton,
    ascii,

    -- * Basic interface
    cons,
    snoc,
    uncons,
    unsnoc,
    length,

    -- * Transformations
    map,
    intercalate,
    intersperse,
    transpose,
    reverse,
    replace,

    -- ** Justification
    justifyLeft,
    justifyRight,
    center,

    -- * Folds
    foldl,
    foldl',
    foldr,
    foldr',

    -- ** Special folds
    concat,
    concatMap,

    -- * Construction

    -- ** Scans
    scanl,
    scanr,

    -- ** Accumulating maps
    mapAccumL,
    mapAccumR,

    -- ** Generation and unfolding
    replicate,
    unfoldr,
    unfoldrN,

    -- * Substrings

    -- ** Breaking strings
    take,
    takeEnd,
    drop,
    dropEnd,
    takeWhile,
    takeWhileEnd,
    dropWhile,
    dropWhileEnd,
    dropAround,
    strip,
    stripStart,
    stripEnd,
    splitAt,
    breakOn,
    breakOnEnd,
    break,
    span,
    group,
    groupBy,
    inits,
    tails,

    -- ** Breaking into many substrings
    splitOn,
    split,
    chunksOf,

    -- ** Breaking into lines and words
    lines,
    unlines,
    words,
    unwords,

    -- * View patterns
    stripPrefix,
    stripSuffix,
    stripInfix,
    -- commonPrefixes,

    -- * Searching
    filter,
    breakOnAll,
    find,
    partition,

    -- * Indexing
    index,
    findIndex,
    count,

    -- * Zipping
    zip,
    zipWith,

    -- * Decoding
    decodeAscii,
    decodeAsciiMay,
    decodeBytesAscii,
    decodeBytesAsciiMay,

    -- * Encoding
    encodeAscii,

    -- * Conversion
    toBytes,
    unpackAscii,
    packAscii,

    -- * Optics
    _AsAscii,
    _AsBytesAscii,
    _PackedAscii,
    chars,
    bytes,

    -- * Low-level
    copy,
  )
where

import Control.Category ((.))
import Control.Monad (foldM, foldM_, when)
import Control.Monad.Primitive (primitive_)
import Control.Monad.ST (ST, runST)
import Data.Bifunctor (bimap)
import Data.Bits
  ( bit,
    complement,
    countTrailingZeros,
    popCount,
    shiftR,
    xor,
    (.|.),
  )
import Data.Bool (Bool (False, True), otherwise, (&&), (||))
import Data.Char (Char, chr, isAscii, ord)
import Data.Coerce (coerce)
import Data.Either (Either (Left))
import Data.Foldable (Foldable (foldMap), traverse_)
import qualified Data.Foldable as F
import Data.Kind (Type)
import qualified Data.List as L
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (mempty)
import Data.Primitive.ByteArray
  ( ByteArray (ByteArray),
    MutableByteArray (MutableByteArray),
    compareByteArrays,
    copyByteArray,
    indexByteArray,
    newByteArray,
    shrinkMutableByteArray,
    unsafeFreezeByteArray,
    writeByteArray,
  )
import Data.Semigroup (stimes)
import Data.Word (Word8)
import GHC.Exts
  ( Int (I#),
    IsList (Item, fromList, fromListN, toList),
    byteSwap64#,
    ctz64#,
    indexWord8ArrayAsWord64#,
    pdep64#,
    writeWord8ArrayAsWord64#,
  )
import GHC.Word (Word64 (W64#))
import Optics.Indexed.Core (itraverse_)
import Optics.Iso (Iso', iso)
import Optics.IxFold (IxFold, ifoldVL)
import Optics.IxTraversal (IxTraversal', itraversalVL, itraverse)
import Optics.Prism (Prism', prism')
import Text.Ascii.Internal (AsciiChar (AsciiChar), AsciiText (AT))
import Text.Ascii.QQ (ascii, char)
import Prelude
  ( pure,
    ($),
    (*),
    (+),
    (-),
    (/=),
    (<),
    (<=),
    (<>),
    (==),
    (>),
    (>=),
  )
import qualified Prelude as P

-- Creation

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XQuasiQuotes
-- >>> :set -XOverloadedStrings
-- >>> import Text.Ascii
-- >>> import Text.Ascii.Char (char, upcase, AsciiCase (Lower), caseOf)
-- >>> import Prelude ((.), ($), (<>), (==), (<), (/=), (-), max, even)
-- >>> import qualified Prelude as Prelude
-- >>> import Data.Maybe (Maybe (Just), fromMaybe)
-- >>> import qualified Data.ByteString as BS
-- >>> import Optics.AffineFold (preview)
-- >>> import Optics.Review (review)
-- >>> import Optics.Getter (view)
-- >>> import Optics.IxTraversal (elementOf)
-- >>> import Optics.IxSetter (iover)
-- >>> import Data.Bool (bool)
-- >>> import Optics.IxFold (itoListOf)

-- | The empty text.
--
-- >>> empty
-- ""
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
empty :: AsciiText
empty = mempty

-- | A text consisting of a single ASCII character.
--
-- >>> singleton [char| 'w' |]
-- "w"
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
singleton :: AsciiChar -> AsciiText
singleton c = fromListN 1 [c]

-- Basic interface

-- | Adds a character to the front of a text. This requires copying, which gives
-- its complexity.
--
-- >>> cons [char| 'n' |] [ascii| "eko" |]
-- "neko"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
cons :: AsciiChar -> AsciiText -> AsciiText
cons c t = fromListN (length t + 1) (c : toList t)

-- | Adds a character to the back of a text. This requires copying, which gives
-- its complexity.
--
-- >>> snoc [ascii| "nek" |] [char| 'o' |]
-- "neko"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
snoc :: AsciiText -> AsciiChar -> AsciiText
snoc t c = fromListN (length t + 1) (toList t <> [c])

-- | If the argument is non-empty, gives 'Just' the first character and the
-- rest, and 'Nothing' otherwise.
--
-- >>> uncons empty
-- Nothing
-- >>> uncons . singleton $ [char| 'w' |]
-- Just ('0x77',"")
-- >>> uncons [ascii| "nekomimi" |]
-- Just ('0x6e',"ekomimi")
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
uncons :: AsciiText -> Maybe (AsciiChar, AsciiText)
uncons (AT ba off len) = case len of
  0 -> Nothing
  _ -> Just (AsciiChar . indexByteArray ba $ off, AT ba (off + 1) (len - 1))

-- | If the argument is non-empty, gives 'Just' the initial segment and the last
-- character, and 'Nothing' otherwise.
--
-- >>> unsnoc empty
-- Nothing
-- >>> unsnoc . singleton $ [char| 'w' |]
-- Just ("",'0x77')
-- >>> unsnoc [ascii| "catboy" |]
-- Just ("catbo",'0x79')
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
unsnoc :: AsciiText -> Maybe (AsciiText, AsciiChar)
unsnoc (AT ba off len) = case len of
  0 -> Nothing
  _ -> Just (AT ba off (len - 1), AsciiChar . indexByteArray ba $ off + len - 1)

-- | The number of characters (and, since this is ASCII, bytes) in the text.
--
-- >>> length . singleton $ [char| 'w' |]
-- 1
-- >>> length [ascii| "nyan nyan" |]
-- 9
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
length :: AsciiText -> Int
length (AT _ _ len) = len

-- Transformations

-- | Copy, and apply the function to each element of, the text.
--
-- >>> map (\c -> fromMaybe c . upcase $ c) [ascii| "nyan!" |]
-- "NYAN!"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
map :: (AsciiChar -> AsciiChar) -> AsciiText -> AsciiText
map f (AT ba off len) = runST $ do
  mba <- newByteArray len
  traverse_ (go mba) [off .. off + len - 1]
  frozen <- unsafeFreezeByteArray mba
  pure . AT frozen 0 $ len
  where
    go :: MutableByteArray s -> Int -> ST s ()
    go mba oldI = do
      let (AsciiChar w8) = f . AsciiChar . indexByteArray ba $ oldI
      writeByteArray mba (oldI - off) w8

-- | Takes a text and a list of texts, and concatenates the list after
-- interspersing the first argument between each element of the list.
--
-- >>> intercalate [ascii| " ~ " |] []
-- ""
-- >>> intercalate [ascii| " ~ " |] [[ascii| "nyan" |]]
-- "nyan"
-- >>> intercalate [ascii| " ~ " |] . Prelude.replicate 3 $ [ascii| "nyan" |]
-- "nyan ~ nyan ~ nyan"
-- >>> intercalate empty . Prelude.replicate 3 $ [ascii| "nyan" |]
-- "nyannyannyan"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
intercalate :: AsciiText -> [AsciiText] -> AsciiText
intercalate (AT ba off len) = \case
  [] -> mempty
  [t] -> t
  ell@(AT ba' off' len' : ts) -> runST $ do
    let sumLens = P.sum . P.fmap length $ ell
    let totalInterLen = len * P.length ts
    mba <- newByteArray (sumLens + totalInterLen)
    copyByteArray mba 0 ba' off' len'
    foldM_ (go mba) len' ts
    frozen <- unsafeFreezeByteArray mba
    pure . AT frozen 0 $ sumLens + totalInterLen
  where
    go :: MutableByteArray s -> Int -> AsciiText -> ST s Int
    go mba pos (AT ba' off' len') = do
      copyByteArray mba pos ba off len
      copyByteArray mba (pos + len) ba' off' len'
      pure (pos + len + len')

-- | Takes a character, and places it between the characters of a text.
--
-- >>> intersperse [char| '~' |] empty
-- ""
-- >>> intersperse [char| '~' |] . singleton $ [char| 'w' |]
-- "w"
-- >>> intersperse [char| '~' |] [ascii| "nyan" |]
-- "n~y~a~n"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
intersperse :: AsciiChar -> AsciiText -> AsciiText
intersperse (AsciiChar w8) at@(AT ba off len) = case len of
  0 -> at
  1 -> at
  _ -> runST $ do
    let newLen = len * 2 - 1
    mba <- newByteArray newLen
    writeByteArray mba 0 . indexByteArray @Word8 ba $ off
    traverse_ (go mba) [1, 3 .. newLen - 1]
    frozen <- unsafeFreezeByteArray mba
    pure . AT frozen 0 $ newLen
  where
    go :: MutableByteArray s -> Int -> ST s ()
    go mba i = do
      writeByteArray mba i w8
      let oldI = (i + 1) `P.quot` 2
      writeByteArray mba (i + 1) . indexByteArray @Word8 ba $ oldI

-- | Transpose the rows and columns of the argument. This uses
-- 'Data.List.transpose' internally, and thus, isn't very efficient.
--
-- >>> transpose []
-- []
-- >>> transpose [[ascii| "w" |]]
-- ["w"]
-- >>> transpose [[ascii| "nyan" |]]
-- ["n","y","a","n"]
-- >>> transpose . Prelude.replicate 3 $ [ascii| "nyan" |]
-- ["nnn","yyy","aaa","nnn"]
-- >>> transpose [[ascii| "cat" |], [ascii| "boy" |], [ascii| "nyan" |]]
-- ["cbn","aoy","tya","n"]
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
transpose :: [AsciiText] -> [AsciiText]
transpose = P.fmap fromList . L.transpose . P.fmap toList

-- | Reverse the text.
--
-- >>> reverse empty
-- ""
-- >>> reverse . singleton $ [char| 'w' |]
-- "w"
-- >>> reverse [ascii| "catboy goes nyan" |]
-- "nayn seog yobtac"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
reverse :: AsciiText -> AsciiText
reverse at@(AT ba@(ByteArray ba#) off len) = case len of
  0 -> at
  1 -> at
  _ -> go
  where
    go :: AsciiText
    go = runST $ do
      mba <- newByteArray len
      let lim = len `P.div` 2
      let countWord64 = lim `P.div` 8
      let word64Ixes = P.fmap (* 8) [0 .. countWord64 - 1]
      traverse_ (bigGo mba) word64Ixes
      traverse_ (littleGo mba) [countWord64 * 8 .. lim - 1]
      frozen <- unsafeFreezeByteArray mba
      pure . AT frozen 0 $ len
    bigGo :: MutableByteArray s -> Int -> ST s ()
    bigGo (MutableByteArray mba#) loIx = do
      let !(I# loIx#) = loIx + off
      let loRead = byteSwap64# (indexWord8ArrayAsWord64# ba# loIx#)
      let !(I# hiIx#) = len - loIx - 8 + off
      let hiRead = byteSwap64# (indexWord8ArrayAsWord64# ba# hiIx#)
      let !(I# dstLoIx#) = loIx
      let !(I# dstHiIx#) = len - loIx - 8
      primitive_ (writeWord8ArrayAsWord64# mba# dstLoIx# hiRead)
      primitive_ (writeWord8ArrayAsWord64# mba# dstHiIx# loRead)
    littleGo :: MutableByteArray s -> Int -> ST s ()
    littleGo mba loIx = do
      let loRead = indexByteArray @Word8 ba (loIx + off)
      let hiIx = len - loIx - 1 + off
      let hiRead = indexByteArray @Word8 ba hiIx
      writeByteArray mba loIx hiRead
      writeByteArray mba (hiIx - off) loRead

-- | @replace needle replacement haystack@, given a @needle@ of length \(n\) and
-- a haystack of length \(h\), replaces each non-overlapping occurrence of
-- @needle@ in @haystack@ with @replacement@. If the @needle@ is empty, no
-- replacement will be performed. Equivalent to @'intercalate' replacement '.'
-- 'splitOn' needle '$' haystack@.
--
-- >>> replace empty [ascii| "NYAN~" |] [ascii| "catboy goes nyan nyan" |]
-- "catboy goes nyan nyan"
-- >>> replace [ascii| "nyan" |] [ascii| "NYAN~" |] empty
-- ""
-- >>> replace [ascii| "nyan" |] [ascii| "NYAN~" |] [ascii| "catboy goes nyan nyan" |]
-- "catboy goes NYAN~ NYAN~"
-- >>> replace [ascii| "nyan" |] [ascii| "NYAN~" |] [ascii| "nyanyan" |]
-- "NYAN~yan"
--
-- = On complexity
--
-- This function is based on a [naive string
-- search](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html),
-- with two added optimizations:
--
-- * We first detect all locations of the first character of the needle; and
-- * Once we find a match, we can skip forward by its length (or more, if the
--   next location is further away).
--
-- The average-case analysis is based on the assumptions that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
--   haystack; and
-- * The needle and haystack together contain at least two unique symbols.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The first symbol of the needle is frequent, but doesn't lead to many
--   matches; or
-- * The needle and/or haystack use few unique symbols.
--
-- Also check the [description of the
-- algorithm](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- for additional pathological case examples.
--
-- The analysis below also doesn't factor in the cost of performing the
-- replacement, as this is (among other things) proportional to the number of
-- matches of the needle (and thus is hard to quantify).
--
-- /Complexity:/ \(\Theta(h - n)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- * ["Algorithms on Strings"](https://www.cambridge.org/core/books/algorithms-on-strings/19049704C876795D95D8882C73257C70) by Crochemore, Hancart and Lecroq. PDF available [here](https://www.researchgate.net/publication/220693689_Algorithms_on_Strings).
--
-- @since 1.0.1
replace ::
  -- | @needle@ to search for
  AsciiText ->
  -- | @replacement@ to replace @needle@ with
  AsciiText ->
  -- | @haystack@ in which to search
  AsciiText ->
  AsciiText
replace needle replacement haystack
  | length needle == 0 || length haystack == 0 = haystack
  | length needle > length haystack = haystack
  | otherwise = intercalate replacement . splitOn needle $ haystack

-- | @justifyLeft n c t@ produces a result of length \(\max \{ {\tt n }, {\tt length} \; {\tt t} \}\),
-- consisting of a copy of @t@ followed by (zero or more) copies
-- of @c@.
--
-- >>> justifyLeft (-100) [char| '~' |] [ascii| "nyan" |]
-- "nyan"
-- >>> justifyLeft 4 [char| '~' |] [ascii| "nyan" |]
-- "nyan"
-- >>> justifyLeft 10 [char| '~' |] [ascii| "nyan" |]
-- "nyan~~~~~~"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
justifyLeft :: Int -> AsciiChar -> AsciiText -> AsciiText
justifyLeft n c t = t <> replicate (n - length t) (singleton c)

-- | @justifyRight n c t@ produces a result of length \(\max \{ {\tt n }, {\tt length} \; {\tt t} \}\),
-- consisting of (zero or more) copies of @c@ followed by a copy of @t@.
--
-- >>> justifyRight (-100) [char| '~' |] [ascii| "nyan" |]
-- "nyan"
-- >>> justifyRight 4 [char| '~' |] [ascii| "nyan" |]
-- "nyan"
-- >>> justifyRight 10 [char| '~' |] [ascii| "nyan" |]
-- "~~~~~~nyan"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
justifyRight :: Int -> AsciiChar -> AsciiText -> AsciiText
justifyRight n c t = replicate (n - length t) (singleton c) <> t

-- | @center n c t@ produces a result of length \({\tt k } = \max \{ {\tt n }, {\tt length} \; {\tt t} \}\),
-- consisting of:
--
-- * \(\lceil \frac{{\tt k} - {\tt length} \; {\tt t}}{2} \rceil\) copies of @c@;
-- followed by
-- * A copy of @t@; followed by
-- * Zero or more copies of @c@
--
-- This means that the centering is \'left-biased\'. This mimicks the behaviour
-- of the function of the same name in the [text
-- package](http://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html#v:center),
-- although that function's documenation does not describe this behaviour.
--
-- >>> center (-100) [char| '~' |] [ascii| "nyan" |]
-- "nyan"
-- >>> center 4 [char| '~' |] [ascii| "nyan" |]
-- "nyan"
-- >>> center 5 [char| '~' |] [ascii| "nyan" |]
-- "~nyan"
-- >>> center 6 [char| '~' |] [ascii| "nyan" |]
-- "~nyan~"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
center :: Int -> AsciiChar -> AsciiText -> AsciiText
center n c t
  | n <= length t = t
  | P.even (n - length t) = copied <> t <> copied
  | otherwise = copied <> singleton c <> t <> copied
  where
    copied :: AsciiText
    copied = replicate ((n - length t) `P.div` 2) (singleton c)

-- Folds

-- | Left-associative fold of a text.
--
-- >>> foldl (\acc c -> [ascii| "f(" |] <> acc <> singleton c <> [ascii| ")" |]) [ascii| "a" |] [ascii| "catboy" |]
-- "f(f(f(f(f(f(ac)a)t)b)o)y)"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
foldl :: (a -> AsciiChar -> a) -> a -> AsciiText -> a
foldl f x = P.foldl f x . toList

-- | Left-associative fold of a text, strict in the accumulator.
--
-- >>> foldl' (\acc c -> [ascii| "f(" |] <> acc <> singleton c <> [ascii| ")" |]) [ascii| "a" |] [ascii| "catboy" |]
-- "f(f(f(f(f(f(ac)a)t)b)o)y)"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
foldl' :: (a -> AsciiChar -> a) -> a -> AsciiText -> a
foldl' f x = F.foldl' f x . toList

-- | Right-associative fold of a text.
--
-- >>> foldr (\c acc -> [ascii| "f(" |] <> acc <> singleton c <> [ascii| ")" |]) [ascii| "a" |] [ascii| "catboy" |]
-- "f(f(f(f(f(f(ay)o)b)t)a)c)"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
foldr :: (AsciiChar -> a -> a) -> a -> AsciiText -> a
foldr f x = P.foldr f x . toList

-- | Right-associative fold of a text, strict in the accumulator.
--
-- >>> foldr' (\c acc -> [ascii| "f(" |] <> acc <> singleton c <> [ascii| ")" |]) [ascii| "a" |] [ascii| "catboy" |]
-- "f(f(f(f(f(f(ay)o)b)t)a)c)"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
foldr' :: (AsciiChar -> a -> a) -> a -> AsciiText -> a
foldr' f x = F.foldr' f x . toList

-- Special folds

-- | Concatenate a list of texts.
--
-- >>> concat []
-- ""
-- >>> concat [[ascii| "catboy" |]]
-- "catboy"
-- >>> concat . Prelude.replicate 4 $ [ascii| "nyan" |]
-- "nyannyannyannyan"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
concat :: [AsciiText] -> AsciiText
concat = P.mconcat

-- | Map a text-producing function over a text, then concatenate the results.
--
-- >>> concatMap singleton empty
-- ""
-- >>> concatMap singleton [ascii| "nyan" |]
-- "nyan"
-- >>> concatMap (\c -> singleton c <> singleton c) [ascii| "nekomimi" |]
-- "nneekkoommiimmii"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
concatMap :: (AsciiChar -> AsciiText) -> AsciiText -> AsciiText
concatMap f = foldMap f . toList

-- | 'scanl' is similar to 'foldl', but returns a list of successive values from
-- the left.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
scanl ::
  -- | accumulator -> element -> new accumulator
  (AsciiChar -> AsciiChar -> AsciiChar) ->
  -- | Starting accumulator value
  AsciiChar ->
  -- | Input of length \(n\)
  AsciiText ->
  -- | Output of length \(n + 1\)
  AsciiText
scanl f x = fromList . P.scanl f x . toList

-- | 'scanr' is similar to 'foldr', but returns a list of successive values from
-- the right.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
scanr ::
  -- | element -> accumulator -> new accumulator
  (AsciiChar -> AsciiChar -> AsciiChar) ->
  -- | Starting accumulator value
  AsciiChar ->
  -- | Input of length \(n\)
  AsciiText ->
  -- | Output of length \(n + 1\)
  AsciiText
scanr f x = fromList . P.scanr f x . toList

-- Accumulating maps

-- | Like a combination of 'map' and 'foldl''. Applies a function to each
-- element of an 'AsciiText', passing an accumulating parameter from left to
-- right, and returns a final 'AsciiText' along with the accumulating
-- parameter's final value.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
mapAccumL :: (a -> AsciiChar -> (a, AsciiChar)) -> a -> AsciiText -> (a, AsciiText)
mapAccumL f x = P.fmap fromList . L.mapAccumL f x . toList

-- | Like a combination of 'map' and 'foldr'. Applies a function to each element
-- of an 'AsciiText', passing an accumulating parameter from right to left, and
-- returns a final 'AsciiText' along with the accumulating parameter's final
-- value.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
mapAccumR :: (a -> AsciiChar -> (a, AsciiChar)) -> a -> AsciiText -> (a, AsciiText)
mapAccumR f x = P.fmap fromList . L.mapAccumR f x . toList

-- Generation and unfolding

-- | @replicate n t@ consists of @t@ repeated \(\max \{ 0, {\tt n } \}\) times.
--
-- >>> replicate (-100) [ascii| "nyan" |]
-- ""
-- >>> replicate 0 [ascii| "nyan" |]
-- ""
-- >>> replicate 3 [ascii| "nyan" |]
-- "nyannyannyan"
--
-- /Complexity:/ \(\Theta(n \cdot m)\)
--
-- @since 1.0.1
replicate :: Int -> AsciiText -> AsciiText
replicate n t
  | n <= 0 = empty
  | otherwise = stimes n t

-- | Similar to 'Data.List.unfoldr'. The function parameter takes a seed value,
-- and produces either 'Nothing' (indicating that we're done) or 'Just' an
-- 'AsciiChar' and a new seed value. 'unfoldr' then, given a starting seed, will
-- repeatedly call the function parameter on successive seed values, returning
-- the resulting 'AsciiText', based on the 'AsciiChar's produced, in the same
-- order.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
unfoldr :: (a -> Maybe (AsciiChar, a)) -> a -> AsciiText
unfoldr f = fromList . L.unfoldr f

-- | Similar to 'unfoldr', but also takes a maximum length parameter. The second
-- element of the result tuple will be 'Nothing' if we finished with the
-- function argument returning 'Nothing', and 'Just' the final seed value if we
-- reached the maximum length before that happened.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
unfoldrN ::
  forall (a :: Type).
  Int ->
  (a -> Maybe (AsciiChar, a)) ->
  a ->
  (AsciiText, Maybe a)
unfoldrN n f x
  | n <= 0 = (empty, Just x)
  | otherwise = runST $ do
    mba <- newByteArray n
    (resultLen, acc) <- foldM (go mba) (0, Just x) [0 .. n - 1]
    when (resultLen < n) (shrinkMutableByteArray mba resultLen)
    frozen <- unsafeFreezeByteArray mba
    pure (AT frozen 0 resultLen, acc)
  where
    go :: MutableByteArray s -> (Int, Maybe a) -> Int -> ST s (Int, Maybe a)
    go mba acc@(_, acc') i = case acc' of
      Nothing -> pure acc
      Just val -> case f val of
        Nothing -> pure (i, Nothing)
        Just (AsciiChar c, val') -> do
          writeByteArray mba i c
          pure (i + 1, Just val')

-- | @take n t@ returns the prefix of @t@ with length
-- \(\min \{ \max \{ 0, {\tt n}\}, {\tt length} \; {\tt t} \}\).
--
-- >>> take (-100) [ascii| "catboy" |]
-- ""
-- >>> take 0 [ascii| "catboy" |]
-- ""
-- >>> take 4 [ascii| "catboy" |]
-- "catb"
-- >>> take 1000 [ascii| "catboy" |]
-- "catboy"
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
take :: Int -> AsciiText -> AsciiText
take n at@(AT ba off len)
  | n <= 0 = mempty
  | n >= len = at
  | otherwise = AT ba off . P.min len $ n

-- | @takeEnd n t@ returns the suffix of @t@ with length
-- \(\min \{ \max \{0, {\tt n} \}, {\tt length} \; {\tt t} \}\).
--
-- >>> takeEnd (-100) [ascii| "catboy" |]
-- ""
-- >>> takeEnd 0 [ascii| "catboy" |]
-- ""
-- >>> takeEnd 4 [ascii| "catboy" |]
-- "tboy"
-- >>> takeEnd 1000 [ascii| "catboy" |]
-- "catboy"
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.1
takeEnd :: Int -> AsciiText -> AsciiText
takeEnd n t = drop (length t - n) t

-- | @drop n t@ returns the suffix of @t@ with length
-- \(\max \{ 0, \min \{ {\tt length} \; {\tt t}, {\tt length} \; {\tt t} - {\tt n} \} \}\).
--
-- >>> drop (-100) [ascii| "catboy" |]
-- "catboy"
-- >>> drop 0 [ascii| "catboy" |]
-- "catboy"
-- >>> drop 4 [ascii| "catboy" |]
-- "oy"
-- >>> drop 1000 [ascii| "catboy" |]
-- ""
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
drop :: Int -> AsciiText -> AsciiText
drop n at@(AT ba off len)
  | n <= 0 = at
  | n >= len = mempty
  | otherwise = AT ba (off + n) (len - n)

-- | @dropEnd n t@ returns the prefix of @t@ with length
-- \(\max \{ 0, \min \{ {\tt length} \; {\tt t}, {\tt length} \; {\tt t} - {\tt n} \} \}\).
--
-- >>> dropEnd (-100) [ascii| "catboy" |]
-- "catboy"
-- >>> dropEnd 0 [ascii| "catboy" |]
-- "catboy"
-- >>> dropEnd 4 [ascii| "catboy" |]
-- "ca"
-- >>> dropEnd 1000 [ascii| "catboy" |]
-- ""
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.1
dropEnd :: Int -> AsciiText -> AsciiText
dropEnd n t = take (length t - n) t

-- | @takeWhile p t@ returns the longest prefix of @t@ of characters that
-- satisfy @p@.
--
-- >>> takeWhile ((Just Lower ==) . caseOf) empty
-- ""
-- >>> takeWhile ((Just Lower ==) . caseOf) [ascii| "catboy goes nyan" |]
-- "catboy"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
takeWhile :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
takeWhile f = fromList . L.takeWhile f . toList

-- | @takeWhileEnd p t@ returns the longest suffix of @t@ of characters that
-- satisfy @p@. Equivalent to @'reverse' . 'takeWhile' p . 'reverse'@.
--
-- >>> takeWhileEnd ((Just Lower ==) . caseOf) empty
-- ""
-- >>> takeWhileEnd ((Just Lower ==) . caseOf) [ascii| "catboy goes nyan" |]
-- "nyan"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
takeWhileEnd :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
takeWhileEnd f = fromList . L.reverse . L.takeWhile f . L.reverse . toList

-- | @dropWhile p t@ returns the suffix remaining after @'takeWhile' p t@.
--
-- >>> dropWhile ((Just Lower ==) . caseOf) empty
-- ""
-- >>> dropWhile ((Just Lower ==) . caseOf) [ascii| "catboy goes nyan" |]
-- " goes nyan"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
dropWhile :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
dropWhile f = fromList . L.dropWhile f . toList

-- | @dropWhileEnd p t@ returns the prefix remaining after @'takeWhileEnd' p t@.
-- Equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
-- >>> dropWhileEnd ((Just Lower ==) . caseOf) empty
-- ""
-- >>> dropWhileEnd ((Just Lower ==) . caseOf) [ascii| "catboy goes nyan" |]
-- "catboy goes "
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
dropWhileEnd :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
dropWhileEnd f = fromList . L.dropWhileEnd f . toList

-- | @dropAround p@ is equivalent to @'dropWhile' p '.' 'dropWhileEnd' p@.
--
-- >>> dropAround ((Just Lower ==) . caseOf) empty
-- ""
-- >>> dropAround ((Just Lower ==) . caseOf) [ascii| "catboy goes nyan" |]
-- " goes "
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
dropAround :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
dropAround p = dropWhile p . dropWhileEnd p

-- | Remove the longest prefix /and/ suffix of the input comprised entirely of
-- whitespace characters. We define a \'whitespace character\' as any of the
-- following:
--
-- * TAB (0x09)
-- * LF (0x0a)
-- * VT (0x0b)
-- * FF (0x0c)
-- * CR (0x0d)
-- * Space (0x20)
--
-- >>> strip empty
-- ""
-- >>> strip [ascii| "catboy goes nyan" |]
-- "catboy goes nyan"
-- >>> strip [ascii| "\n\n    \tcatboy goes nyan" |]
-- "catboy goes nyan"
-- >>> strip [ascii| "catboy goes nyan   \t\t\n" |]
-- "catboy goes nyan"
-- >>> strip [ascii| "\n\n    \tcatboy goes nyan   \t\t\n" |]
-- "catboy goes nyan"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
strip :: AsciiText -> AsciiText
strip = dropAround isSpace

-- | Remove the longest prefix of the input comprised entirely of whitespace
-- characters. We define a \'whitespace character\' as any of the following:
--
-- * TAB (0x09)
-- * LF (0x0a)
-- * VT (0x0b)
-- * FF (0x0c)
-- * CR (0x0d)
-- * Space (0x20)
--
-- >>> stripStart empty
-- ""
-- >>> stripStart [ascii| "catboy goes nyan" |]
-- "catboy goes nyan"
-- >>> stripStart [ascii| "\n\n    \tcatboy goes nyan" |]
-- "catboy goes nyan"
-- >>> stripStart [ascii| "catboy goes nyan   \t\t\n" |]
-- "catboy goes nyan   \t\t\n"
-- >>> stripStart [ascii| "\n\n    \tcatboy goes nyan   \t\t\n" |]
-- "catboy goes nyan   \t\t\n"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
stripStart :: AsciiText -> AsciiText
stripStart = dropWhile isSpace

-- | Remove the longest suffix of the input comprised entirely of whitespace
-- characters. We define a \'whitespace character\' as any of the following:
--
-- * TAB (0x09)
-- * LF (0x0a)
-- * VT (0x0b)
-- * FF (0x0c)
-- * CR (0x0d)
-- * Space (0x20)
--
-- >>> stripEnd empty
-- ""
-- >>> stripEnd [ascii| "catboy goes nyan" |]
-- "catboy goes nyan"
-- >>> stripEnd [ascii| "\n\n    \tcatboy goes nyan" |]
-- "\n\n    \tcatboy goes nyan"
-- >>> stripEnd [ascii| "catboy goes nyan   \t\t\n" |]
-- "catboy goes nyan"
-- >>> stripEnd [ascii| "\n\n    \tcatboy goes nyan   \t\t\n" |]
-- "\n\n    \tcatboy goes nyan"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
stripEnd :: AsciiText -> AsciiText
stripEnd = dropWhileEnd isSpace

-- | @splitAt n t@ is equivalent to @('take' n t, 'drop' n t)@.
--
-- >>> splitAt (-3) [ascii| "catboy" |]
-- ("","catboy")
-- >>> splitAt 0 [ascii| "catboy" |]
-- ("","catboy")
-- >>> splitAt 3 [ascii| "catboy" |]
-- ("cat","boy")
-- >>> splitAt 1000 [ascii| "catboy" |]
-- ("catboy","")
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
splitAt :: Int -> AsciiText -> (AsciiText, AsciiText)
splitAt i at@(AT ba off len)
  | i <= 0 = (mempty, at)
  | i < len = (AT ba off i, AT ba (off + i) (len - i))
  | otherwise = (at, mempty)

-- | @breakOn needle haystack@, given a @needle@ of length \(n\) and a
-- @haystack@ of length \(h\), attempts to find the first instance of @needle@
-- in @haystack@. If successful, return a tuple consisting of:
--
-- * The prefix of @haystack@ before the match; and
-- * The rest of @haystack@, starting with the match.
--
-- If the needle is empty, this returns @('empty', haystack)@. If no match can
-- be found, this instead returns @(haystack, 'empty')@.
--
-- If you need to repeatedly split on the same needle, consider 'breakOnAll', as
-- this will be more efficient due to only having to run the matching algorithm
-- once.
--
-- >>> breakOn empty [ascii| "catboy goes nyan" |]
-- ("","catboy goes nyan")
-- >>> breakOn [ascii| "nyan" |] empty
-- ("","")
-- >>> breakOn [ascii| "goes" |] [ascii| "catboy goes nyan" |]
-- ("catboy ","goes nyan")
-- >>> breakOn [ascii| "catboy" |] [ascii| "nyan nyan nyan" |]
-- ("nyan nyan nyan","")
--
-- = On complexity
--
-- This function is based on a [naive string
-- search](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html),
-- with two added optimizations:
--
-- * We first detect all locations of the first character of the needle; and
-- * Once we find a match, we can skip forward by its length (or more, if the
--   next location is further away).
--
-- The average-case analysis is based on the assumptions that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
--   haystack; and
-- * The needle and haystack together contain at least two unique symbols.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The first symbol of the needle is frequent, but doesn't lead to many
--   matches; or
-- * The needle and/or haystack use few unique symbols.
--
-- Also check the [description of the
-- algorithm](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- for additional pathological case examples.
--
-- /Complexity:/ \(\Theta(h - n)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- * ["Algorithms on Strings"](https://www.cambridge.org/core/books/algorithms-on-strings/19049704C876795D95D8882C73257C70) by Crochemore, Hancart and Lecroq. PDF available [here](https://www.researchgate.net/publication/220693689_Algorithms_on_Strings).
--
-- @since 1.0.1
breakOn :: AsciiText -> AsciiText -> (AsciiText, AsciiText)
breakOn needle@(AT nba noff nlen) haystack@(AT hba hoff hlen)
  | length needle == 0 = (empty, haystack)
  | otherwise = case indices nba noff nlen hba hoff hlen of
    [] -> (haystack, empty)
    ix : _ -> splitAt ix haystack

-- | @breakOnEnd needle haystack@, given a @needle@ of length \(n\) and a
-- @haystack@ of length \(h\), attempts to find the last instance of @needle@ in
-- @haystack@. If successful, return a tuple consisting of:
--
-- * The prefix of @haystack@ up to, and including, the match; and
-- * The rest of @haystack@.
--
-- If the needle is empty, this returns @(haystack, 'empty')@. If no match can
-- be found, this instead returns @('empty', haystack)@.
--
-- This function is similar to 'breakOn'. If you need to repeatedly split on the
-- same needle, consider 'breakOnAll', as this will be more efficient due to
-- only having to run the matching algorithm once.
--
-- >>> breakOnEnd empty [ascii| "catboy goes nyan" |]
-- ("catboy goes nyan","")
-- >>> breakOnEnd [ascii| "nyan" |] empty
-- ("","")
-- >>> breakOnEnd [ascii| "goes" |] [ascii| "catboy goes nyan" |]
-- ("catboy goes"," nyan")
-- >>> breakOnEnd [ascii| "catboy" |] [ascii| "nyan nyan nyan" |]
-- ("","nyan nyan nyan")
--
-- = On complexity
--
-- This function is based on a [naive string
-- search](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html),
-- with two added optimizations:
--
-- * We first detect all locations of the first character of the needle; and
-- * Once we find a match, we can skip forward by its length (or more, if the
--   next location is further away).
--
-- The average-case analysis is based on the assumptions that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
--   haystack; and
-- * The needle and haystack together contain at least two unique symbols.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The first symbol of the needle is frequent, but doesn't lead to many
--   matches; or
-- * The needle and/or haystack use few unique symbols.
--
-- Also check the [description of the
-- algorithm](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- for additional pathological case examples.
--
-- /Complexity:/ \(\Theta(h - n)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- * ["Algorithms on Strings"](https://www.cambridge.org/core/books/algorithms-on-strings/19049704C876795D95D8882C73257C70) by Crochemore, Hancart and Lecroq. PDF available [here](https://www.researchgate.net/publication/220693689_Algorithms_on_Strings).
--
-- @since 1.0.1
breakOnEnd :: AsciiText -> AsciiText -> (AsciiText, AsciiText)
breakOnEnd needle@(AT nba noff nlen) haystack@(AT hba hoff hlen)
  | length needle == 0 = (haystack, empty)
  | otherwise = case go . indices nba noff nlen hba hoff $ hlen of
    Nothing -> (empty, haystack)
    Just ix -> splitAt (ix + nlen) haystack
  where
    go :: [Int] -> Maybe Int
    go = \case
      [] -> Nothing
      [i] -> Just i
      (_ : is) -> go is

-- | @break p t@ is equivalent to @('takeWhile' ('not' p) t, 'dropWhile' ('not'
-- p) t)@.
--
-- >>> break ([char| ' ' |] ==) [ascii| "catboy goes nyan" |]
-- ("catboy"," goes nyan")
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
break :: (AsciiChar -> Bool) -> AsciiText -> (AsciiText, AsciiText)
break f = bimap fromList fromList . P.break f . toList

-- | @span p t@ is equivalent to @('takeWhile' p t, 'dropWhile' p t)@.
--
-- >>> span ([char| 'c' |] ==) [ascii| "catboy goes nyan" |]
-- ("c","atboy goes nyan")
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
span :: (AsciiChar -> Bool) -> AsciiText -> (AsciiText, AsciiText)
span f = bimap fromList fromList . P.span f . toList

-- | Separate a text into a list of texts such that:
--
-- * Their concatenation is equal to the original argument; and
-- * Equal adjacent characters in the original argument are in the same text in
-- the result.
--
-- >>> group empty
-- []
-- >>> group . singleton $ [char| 'w' |]
-- ["w"]
-- >>> group [ascii| "nyan" |]
-- ["n","y","a","n"]
-- >>> group [ascii| "nyaaaan" |]
-- ["n","y","aaaa","n"]
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
group :: AsciiText -> [AsciiText]
group = P.fmap fromList . L.group . toList

-- | Separate a text into a list of texts such that:
--
-- * Their concatenation is equal to the original argument; and
-- * Adjacent characters for which the function argument returns @True@ are in
-- the same text in the result.
--
-- >>> groupBy (<) empty
-- []
-- >>> groupBy (<) . singleton $ [char| 'w' |]
-- ["w"]
-- >>> groupBy (<) [ascii| "catboy goes nyan" |]
-- ["c","atboy"," goes"," nyan"]
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
groupBy :: (AsciiChar -> AsciiChar -> Bool) -> AsciiText -> [AsciiText]
groupBy f = P.fmap fromList . L.groupBy f . toList

-- | All prefixes of the argument, from shortest to longest.
--
-- >>> inits empty
-- [""]
-- >>> inits . singleton $ [char| 'w' |]
-- ["","w"]
-- >>> inits [ascii| "nyan" |]
-- ["","n","ny","nya","nyan"]
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
inits :: AsciiText -> [AsciiText]
inits (AT ba off len) = P.fmap (AT ba off) [0 .. len]

-- | All suffixes of the argument, from shortest to longest.
--
-- >>> tails empty
-- [""]
-- >>> tails . singleton $ [char| 'w' |]
-- ["w",""]
-- >>> tails [ascii| "nyan" |]
-- ["nyan","yan","an","n",""]
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
tails :: AsciiText -> [AsciiText]
tails (AT ba off len) = P.zipWith (AT ba) [off .. off + len - 1] [len, len - 1 .. 0]

-- Breaking into many substrings

-- | @splitOn needle haystack@, given a @needle@ of length \(n\) and a haystack
-- of length \(h\), breaks @haystack@ into pieces, separated by @needle@. Any
-- occurrences of @needle@ in @haystack@ are consumed.
--
-- >>> splitOn empty [ascii| "catboy goes nyan and goes nyan" |]
-- ["catboy goes nyan and goes nyan"]
-- >>> splitOn [ascii| "nyan" |] empty
-- [""]
-- >>> splitOn [ascii| "nyan" |] [ascii| "catboy goes nyan and goes nyan" |]
-- ["catboy goes "," and goes ",""]
-- >>> splitOn [ascii| "nyan" |] [ascii| "nyan" |]
-- ["",""]
-- >>> splitOn [ascii| "nyan" |] [ascii| "catboy" |]
-- ["catboy"]
--
-- = On complexity
--
-- This function is based on a [naive string
-- search](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html),
-- with two added optimizations:
--
-- * We first detect all locations of the first character of the needle; and
-- * Once we find a match, we can skip forward by its length (or more, if the
--   next location is further away).
--
-- The average-case analysis is based on the assumptions that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
--   haystack; and
-- * The needle and haystack together contain at least two unique symbols.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The first symbol of the needle is frequent, but doesn't lead to many
--   matches; or
-- * The needle and/or haystack use few unique symbols.
--
-- Also check the [description of the
-- algorithm](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- for additional pathological case examples.
--
-- /Complexity:/ \(\Theta(h - n)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- * ["Algorithms on Strings"](https://www.cambridge.org/core/books/algorithms-on-strings/19049704C876795D95D8882C73257C70) by Crochemore, Hancart and Lecroq. PDF available [here](https://www.researchgate.net/publication/220693689_Algorithms_on_Strings).
--
-- @since 1.0.1
splitOn :: AsciiText -> AsciiText -> [AsciiText]
splitOn needle@(AT nba noff nlen) haystack@(AT hba hoff hlen)
  | length needle == 0 = [haystack]
  | length haystack == 0 = [empty]
  | otherwise = go 0 . indices nba noff nlen hba hoff $ hlen
  where
    go :: Int -> [Int] -> [AsciiText]
    go pos = \case
      [] -> [drop pos haystack]
      (ix : ixes) ->
        let chunkLen = ix - pos
            segment = take chunkLen . drop pos $ haystack
         in segment : go (pos + chunkLen + nlen) ixes

-- | @split p t@ separates @t@ into components delimited by separators, for
-- which @p@ returns @True@. The results do not contain the separators.
--
-- \(n\) adjacent separators result in \(n - 1\) empty components in the result.
--
-- >>> split ([char| '~' |] ==) empty
-- []
-- >>> split ([char| '~' |] ==) . singleton $ [char| '~' |]
-- ["",""]
-- >>> split ([char| '~' |] ==) [ascii| "nyan" |]
-- ["nyan"]
-- >>> split ([char| '~' |] ==) [ascii| "~nyan" |]
-- ["","nyan"]
-- >>> split ([char| '~' |] ==) [ascii| "nyan~" |]
-- ["nyan",""]
-- >>> split ([char| '~' |] ==) [ascii| "nyan~nyan"|]
-- ["nyan","nyan"]
-- >>> split ([char| '~' |] ==) [ascii| "nyan~~nyan" |]
-- ["nyan","","nyan"]
-- >>> split ([char| '~' |] ==) [ascii| "nyan~~~nyan" |]
-- ["nyan","","","nyan"]
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
split :: (AsciiChar -> Bool) -> AsciiText -> [AsciiText]
split f = P.fmap fromList . go . toList
  where
    go :: [AsciiChar] -> [[AsciiChar]]
    go = \case
      [] -> [[]]
      (x : xs) ->
        if f x
          then []
          else case go xs of
            [] -> P.fail "Impossible split"
            (y : ys) -> (x : y) : ys

-- | Splits a text into chunks of the specified length. Equivalent to repeatedly
-- 'take'ing the specified length until exhaustion. The last item in the result
-- may thus be shorter than requested.
--
-- For any @n <= 0@ and any @t@, @chunksOf n t@ yields the empty list. This is
-- identical to the behaviour of the function of the same name in the [text
-- package](http://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html#v:chunksOf),
-- although it doesn't document this fact.
--
-- >>> chunksOf (-100) [ascii| "I am a catboy" |]
-- []
-- >>> chunksOf (-100) empty
-- []
-- >>> chunksOf 0 [ascii| "I am a catboy" |]
-- []
-- >>> chunksOf 0 empty
-- []
-- >>> chunksOf 1 [ascii| "I am a catboy" |]
-- ["I"," ","a","m"," ","a"," ","c","a","t","b","o","y"]
-- >>> chunksOf 1 empty
-- []
-- >>> chunksOf 2 [ascii| "I am a catboy" |]
-- ["I ","am"," a"," c","at","bo","y"]
-- >>> chunksOf 300 [ascii| "I am a catboy" |]
-- ["I am a catboy"]
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
chunksOf :: Int -> AsciiText -> [AsciiText]
chunksOf n t
  | n <= 0 = []
  | t == empty = []
  | otherwise = case splitAt n t of
    (h, t') -> h : chunksOf n t'

-- Breaking into lines and words

-- | Identical to the functions of the same name in the [text
-- package](http://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html#v:lines),
-- and [the
-- Prelude](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:lines).
-- Specifically, separates the argument into pieces, with LF characters (0x0a) as
-- separators. A single trailing LF is ignored. None of the final results
-- contain LF.
--
-- We chose to follow the same semantics for this function as the text package
-- and the Prelude. This has some consequences,
-- which the documentation of both the text package and the Prelude does not
-- properly explain. We list them here - bear these in mind when using this
-- function, as well as 'unlines':
--
-- * No platform-specific concept of a \'newline\' is ever used by this
-- function. Separation is done on LF, and /only/ LF, regardless of platform.
-- The documentation in both the text package and the Prelude confusingly refers
-- to \'newline characters\', which is a category error. We thus specify that LF
-- is the character being split on, rather than mentioning \'newlines\' in any
-- way, shape or form.
-- * @'unlines' '.' 'lines'@ is /not/ the same as @'Prelude.id'@. This is
-- misleadingly described in the Prelude, which claims that (its version of)
-- @unlines@ is \'an inverse operation\' to (its version of) @lines@. For a
-- precise explanation of why this is the case, please see the documentation for
-- 'unlines'.
-- * @'lines'@ is not the same as @'split' (['char'| \'\n\' |] '==')@. See the
-- doctests below for a demonstration of how they differ.
--
-- >>> lines empty
-- []
-- >>> split ([char| '\n' |] ==) empty
-- []
-- >>> lines [ascii| "catboy goes nyan" |]
-- ["catboy goes nyan"]
-- >>> split ([char| '\n' |] ==) [ascii| "catboy goes nyan" |]
-- ["catboy goes nyan"]
-- >>> lines [ascii| "catboy goes nyan\n" |]
-- ["catboy goes nyan"]
-- >>> split ([char| '\n' |] ==) [ascii| "catboy goes nyan\n" |]
-- ["catboy goes nyan",""]
-- >>> lines [ascii| "\ncatboy\n\n\ngoes\n\nnyan\n\n" |]
-- ["","catboy","","","goes","","nyan",""]
-- >>> split ([char| '\n' |] ==) [ascii| "\ncatboy\n\n\ngoes\n\nnyan\n\n" |]
-- ["","catboy","","","goes","","nyan","",""]
-- >>> lines [ascii| "\r\ncatboy\r\ngoes\r\nnyan\r\n" |]
-- ["\r","catboy\r","goes\r","nyan\r"]
-- >>> split ([char| '\n' |] ==) [ascii| "\r\ncatboy\r\ngoes\r\nnyan\r\n" |]
-- ["\r","catboy\r","goes\r","nyan\r",""]
--
-- /Complexity:/ \(\Theta(n)\)
--
-- /See also:/ [Wikipedia on newlines](https://en.wikipedia.org/wiki/Newline)
--
-- @since 1.0.1
lines :: AsciiText -> [AsciiText]
lines at = case uncons at of
  Nothing -> []
  Just _ -> case break ([char| '\n' |] ==) at of
    (h, t) ->
      h : case uncons t of
        Nothing -> []
        Just (_, at') -> lines at'

-- | Identical to the functions of the same name in the [text
-- package](http://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html#v:words)
-- and [the
-- Prelude](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:words).
-- Specifically, separates the argument into pieces, with (non-empty sequences
-- of) word separator characters as separators. A \'word separator character\'
-- is any of the following:
--
-- * TAB (0x09)
-- * LF (0x0a)
-- * VT (0x0b)
-- * FF (0x0c)
-- * CR (0x0d)
-- * Space (0x20)
--
-- None of the final results contain any word separator characters. Any sequence
-- of leading, or trailing, word separator characters will be ignored.
--
-- We chose to follow the same semantics for this function as the text package
-- and the Prelude. This has the consequence that @'unwords' '.' 'words'@ is
-- /not/ the same as 'Prelude.id', although the documentation for the Prelude
-- confusingly describes (its version of) @unwords@ as an \'inverse operation\'
-- to (its version of) @words@. See the documentation for 'unwords' for an
-- explanation of why this is the case.
--
-- >>> words empty
-- []
-- >>> words [ascii| "catboy" |]
-- ["catboy"]
-- >>> words [ascii| "  \r\r\r\rcatboy   \n\rgoes\t\t\t\t\tnyan\n  " |]
-- ["catboy","goes","nyan"]
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
words :: AsciiText -> [AsciiText]
words at =
  let at' = dropWhile isSep at
   in case length at' of
        0 -> []
        _ -> case break isSep at' of
          (h, t) -> h : words t
  where
    isSep :: AsciiChar -> Bool
    isSep = \case
      [char| '\t' |] -> True
      [char| '\n' |] -> True
      [char| '\v' |] -> True
      [char| '\f' |] -> True
      [char| '\r' |] -> True
      [char| ' ' |] -> True
      _ -> False

{-
words (AsciiText bs) = coerce . go $ bs
  where
    go :: ByteString -> [ByteString]
    go rest =
      let rest' = BS.dropWhile isSep rest
       in case BS.length rest' of
            0 -> []
            _ -> case BS.break isSep rest' of
              (h, t) -> h : go t
    isSep :: Word8 -> Bool
    isSep w8
      | w8 == 32 = True
      | 9 <= w8 && w8 <= 13 = True
      | otherwise = False
-}

-- | Identical to the functions of the same name in the [text
-- package](http://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html#v:unlines)
-- and [the
-- Prelude](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:unlines).
-- Specifically, appends an LF character to each of the texts, then concatenates. Equivalent
-- to @'foldMap' (`'snoc'` [char| '\n' |])@.
--
-- We chose to follow the same semantics for this function as the text package
-- and the Prelude. This has some consequences, which the documentation of both
-- the text package and the Prelude does not properly explain. We list them here
-- - bear these in mind when using this function, as well as 'lines':
--
-- * No platform-specific concept of a \'newline\' is ever used by this
-- function. The documentation in both the text package and the Prelude
-- confusing refer to appending a \'terminating newline\', which is only a
-- correct statement on platforms where a newline is LF. We thus specify that we
-- append LF, rather than mentioning \'newlines\' in any way, shape or form.
-- * @'unlines' '.' 'lines'@ is /not/ the same as @'Prelude.id'@. This is
-- misleadingly described in the Prelude, which claims that (its version of)
-- @unlines@ is \'an inverse operation\' to (its version of) @lines@. See the
-- doctests below for a demonstration of this.
--
-- >>> unlines []
-- ""
-- >>> unlines [[ascii| "nyan" |]]
-- "nyan\n"
-- >>> unlines . Prelude.replicate 3 $ [ascii| "nyan" |]
-- "nyan\nnyan\nnyan\n"
-- >>> unlines . lines $ [ascii| "catboy goes nyan" |]
-- "catboy goes nyan\n"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- /See also:/ [Wikipedia on newlines](https://en.wikipedia.org/wiki/Newline)
--
-- @since 1.0.1
unlines :: (Foldable f) => f AsciiText -> AsciiText
unlines = foldMap (`snoc` [char| '\n' |])

-- | Identical to the functions of the same name in the [text
-- package](http://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html#v:unwords)
-- and [the
-- Prelude](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:unwords).
-- Specifically, links together adjacent texts with a Space character. Equivalent to
-- @'intercalate' [ascii| " " |]@.
--
-- We chose to follow the same semantics for this function as the text package
-- and the Prelude. This has the consequence that @'unwords' '.' 'words'@ is
-- /not/ the same as 'Prelude.id', although the documentation for the Prelude
-- confusingly describes (its version of) @unwords@ as an \'inverse operation\'
-- to (its version of) @words@. See the doctests below for a demonstration of
-- this.
--
-- >>> unwords []
-- ""
-- >>> unwords [[ascii| "nyan" |]]
-- "nyan"
-- >>> unwords . Prelude.replicate 3 $ [ascii| "nyan" |]
-- "nyan nyan nyan"
-- >>> unwords . words $ [ascii| "nyan\nnyan\nnyan" |]
-- "nyan nyan nyan"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
unwords :: [AsciiText] -> AsciiText
unwords = intercalate [ascii| " " |]

-- View patterns

-- | Return 'Just' the suffix of the second text if it has the first text as
-- a prefix, 'Nothing' otherwise.
--
-- >>> stripPrefix [ascii| "catboy" |] empty
-- Nothing
-- >>> stripPrefix empty [ascii| "catboy" |]
-- Just "catboy"
-- >>> stripPrefix [ascii| "nyan" |] [ascii| "nyan" |]
-- Just ""
-- >>> stripPrefix [ascii| "nyan" |] [ascii| "catboy" |]
-- Nothing
-- >>> stripPrefix [ascii| "catboy" |] [ascii| "catboy goes nyan" |]
-- Just " goes nyan"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
stripPrefix :: AsciiText -> AsciiText -> Maybe AsciiText
stripPrefix (AT ba off len) at'@(AT ba' off' len')
  | len == 0 = Just at'
  | len' == 0 = Nothing
  | len > len' = Nothing
  | otherwise = case compareByteArrays ba off ba' off' len of
    P.EQ -> Just . AT ba' (off' + len) $ len' - len
    _ -> Nothing

-- | Return 'Just' the prefix of the second text if it has the first text as
-- a suffix, 'Nothing' otherwise.
--
-- >>> stripSuffix [ascii| "catboy" |] empty
-- Nothing
-- >>> stripSuffix empty [ascii| "catboy" |]
-- Just "catboy"
-- >>> stripSuffix [ascii| "nyan" |] [ascii| "nyan" |]
-- Just ""
-- >>> stripSuffix [ascii| "nyan" |] [ascii| "catboy" |]
-- Nothing
-- >>> stripSuffix [ascii| "nyan" |] [ascii| "catboy goes nyan" |]
-- Just "catboy goes "
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
stripSuffix :: AsciiText -> AsciiText -> Maybe AsciiText
stripSuffix (AT ba off len) at'@(AT ba' off' len')
  | len' == 0 = Nothing
  | len == 0 = Just at'
  | len > len' = Nothing
  | otherwise = case compareByteArrays ba off ba' (off' + len' - len) len of
    P.EQ -> Just . AT ba' off' $ len' - len
    _ -> Nothing

-- | @stripInfix needle haystack@, given a needle of length \(n\) and a haystack
-- of length \(h\), attempts to find the first instance of @needle@ in
-- @haystack@. If successful, it returns 'Just' the pair consisting of:
--
-- * All the text in @haystack@ before the first instance of @needle@; and
-- * All the text in @haystack@ after, but not including, the first instance of
-- @needle@.
--
-- If there is no instance of @needle@ in @haystack@, this returns 'Nothing'.
--
-- >>> stripInfix [ascii| "catboy" |] empty
-- Nothing
-- >>> stripInfix empty [ascii| "nyan catboy nyan nyan" |]
-- Nothing
-- >>> stripInfix [ascii| "catboy" |] [ascii| "catboy" |]
-- Just ("","")
-- >>> stripInfix [ascii| "catboy" |] [ascii| "nyan catboy" |]
-- Just ("nyan ","")
-- >>> stripInfix [ascii| "catboy" |] [ascii| "catboy nyan" |]
-- Just (""," nyan")
-- >>> stripInfix [ascii| "catboy" |] [ascii| "nyan catboy nyan nyan" |]
-- Just ("nyan "," nyan nyan")
-- >>> stripInfix [ascii| "nyan" |] [ascii| "nyanyanyan" |]
-- Just ("","yanyan")
--
-- = On complexity
--
-- This function is based on a [naive string
-- search](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html),
-- with two added optimizations:
--
-- * We first detect all locations of the first character of the needle; and
-- * Once we find a match, we can skip forward by its length (or more, if the
--   next location is further away).
--
-- The average-case analysis is based on the assumptions that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
--   haystack; and
-- * The needle and haystack together contain at least two unique symbols.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The first symbol of the needle is frequent, but doesn't lead to many
--   matches; or
-- * The needle and/or haystack use few unique symbols.
--
-- Also check the [description of the
-- algorithm](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- for additional pathological case examples.
--
-- /Complexity:/ \(\Theta(h - n)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- * ["Algorithms on Strings"](https://www.cambridge.org/core/books/algorithms-on-strings/19049704C876795D95D8882C73257C70) by Crochemore, Hancart and Lecroq. PDF available [here](https://www.researchgate.net/publication/220693689_Algorithms_on_Strings).
--
-- @since 1.0.1
stripInfix :: AsciiText -> AsciiText -> Maybe (AsciiText, AsciiText)
stripInfix (AT nba noff nlen) haystack@(AT hba hoff hlen)
  | P.min nlen hlen == 0 = Nothing
  | otherwise = case indices nba noff nlen hba hoff hlen of
    [] -> Nothing
    (ix : _) -> Just (take ix haystack, drop (ix + nlen) haystack)

{-
-- | Find the longest non-empty common prefix of the arguments and return it,
-- along with the remaining suffixes of both arguments. If the arguments lack a
-- common, non-empty prefix, returns 'Nothing'.
--
-- >>> commonPrefixes empty [ascii| "catboy" |]
-- Nothing
-- >>> commonPrefixes [ascii| "catboy" |] empty
-- Nothing
-- >>> commonPrefixes [ascii| "catboy" |] [ascii| "nyan" |]
-- Nothing
-- >>> commonPrefixes [ascii| "catboy" |] [ascii| "catboy" |]
-- Just ("catboy","","")
-- >>> commonPrefixes [ascii| "nyan" |] [ascii| "nyan nyan" |]
-- Just ("nyan",""," nyan")
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
commonPrefixes :: AsciiText -> AsciiText -> Maybe (AsciiText, AsciiText, AsciiText)
commonPrefixes (AT ba off len) (AT ba' off' len')
  | P.min len len' == 0 = Nothing
  | otherwise = _

commonPrefixes (AsciiText t1) (AsciiText t2) =
  go2 <$> F.foldl' go Nothing [0 .. P.min (BS.length t1) (BS.length t2) - 1]
  where
    go :: Maybe Int -> Int -> Maybe Int
    go acc i
      | BS.index t1 i == BS.index t2 i = Just i
      | otherwise = acc
    go2 :: Int -> (AsciiText, AsciiText, AsciiText)
    go2 i = case BS.splitAt (i + 1) t1 of
      (h, t) -> coerce (h, t, BS.drop (i + 1) t2)
-}

-- Searching

-- | Return the text comprised of all the characters that satisfy the function
-- argument (that is, for which it returns 'True'), in the same order as in the
-- original.
--
-- >>> filter ([char| 'n' |] ==) empty
-- ""
-- >>> filter ([char| 'n' |] ==) [ascii| "catboy" |]
-- ""
-- >>> filter ([char| 'n' |] ==) [ascii| "nyan" |]
-- "nn"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
filter :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
filter f = fromList . L.filter f . toList

-- | @breakOnAll needle haystack@, given a @needle@ of length \(n\) and a
-- @haystack@ of length \(h\), finds all non-overlapping instances of @needle@
-- in @haystack@. Each result consists of the following elements:
--
-- * The prefix prior to the match; and
-- * The match, followed by the rest of the string.
--
-- If given an empty needle, the result is a singleton list containing a pair of
-- the entire haystack and the empty text. If given an empty haystack, the
-- result is an empty list.
--
-- >>> breakOnAll empty [ascii| "nyan nyan nyan" |]
-- [("nyan nyan nyan","")]
-- >>> breakOnAll [ascii| "nyan" |] empty
-- []
-- >>> breakOnAll [ascii| "nyan" |] [ascii| "nyan" |]
-- [("","nyan")]
-- >>> breakOnAll [ascii| "nyan" |] [ascii| "nyan nyan nyan" |]
-- [("","nyan nyan nyan"),("nyan ","nyan nyan"),("nyan nyan ","nyan")]
-- >>> breakOnAll [ascii| "nyan" |] [ascii| "nyanyanyan" |]
-- [("","nyanyanyan"),("nyanya","nyan")]
--
-- = On complexity
--
-- This function is based on a [naive string
-- search](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html),
-- with two added optimizations:
--
-- * We first detect all locations of the first character of the needle; and
-- * Once we find a match, we can skip forward by its length (or more, if the
--   next location is further away).
--
-- The average-case analysis is based on the assumptions that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
--   haystack; and
-- * The needle and haystack together contain at least two unique symbols.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The first symbol of the needle is frequent, but doesn't lead to many
--   matches; or
-- * The needle and/or haystack use few unique symbols.
--
-- Also check the [description of the
-- algorithm](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- for additional pathological case examples.
--
-- /Complexity:/ \(\Theta(h - n)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- * ["Algorithms on Strings"](https://www.cambridge.org/core/books/algorithms-on-strings/19049704C876795D95D8882C73257C70) by Crochemore, Hancart and Lecroq. PDF available [here](https://www.researchgate.net/publication/220693689_Algorithms_on_Strings).
--
-- @since 1.0.1
breakOnAll :: AsciiText -> AsciiText -> [(AsciiText, AsciiText)]
breakOnAll (AT needleBa needleOff needleLen) haystack@(AT haystackBa haystackOff haystackLen)
  | needleLen == 0 = [(haystack, empty)]
  | haystackLen == 0 = []
  | otherwise =
    P.fmap (`splitAt` haystack)
      . indices needleBa needleOff needleLen haystackBa haystackOff
      $ haystackLen

{-
breakOnAll = _

breakOnAll needle@(AsciiText n) haystack@(AsciiText h)
  | length needle == 0 = [(haystack, empty)]
  | length haystack == 0 = []
  | otherwise = (`splitAt` haystack) <$> indices n h
-}

-- | Returns 'Just' the first character in the text satisfying the predicate,
-- 'Nothing' otherwise.
--
-- >>> find ([char| 'n' |] ==) empty
-- Nothing
-- >>> find ([char| 'n' |] ==) [ascii| "catboy" |]
-- Nothing
-- >>> find ([char| 'n' |] ==) [ascii| "nyan" |]
-- Just '0x6e'
-- >>> find ([char| 'n' |] /=) [ascii| "nyan" |]
-- Just '0x79'
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
find :: (AsciiChar -> Bool) -> AsciiText -> Maybe AsciiChar
find f = F.foldl' go Nothing . toList
  where
    go :: Maybe AsciiChar -> AsciiChar -> Maybe AsciiChar
    go acc c = case acc of
      Nothing -> if f c then Just c else Nothing
      Just _ -> acc

-- | @partition p t@ is equivalent to @('filter' p t, 'filter' ('not' p) t)@.
--
-- >>> partition ([char| 'n' |] ==) empty
-- ("","")
-- >>> partition ([char| 'n' |] ==) . singleton $ [char| 'n' |]
-- ("n","")
-- >>> partition ([char| 'n' |] ==) . singleton $ [char| 'w' |]
-- ("","w")
-- >>> partition ([char| 'n' |] ==) [ascii| "nyan!" |]
-- ("nn","ya!")
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
partition :: (AsciiChar -> Bool) -> AsciiText -> (AsciiText, AsciiText)
partition f t = (filter f t, filter (P.not . f) t)

-- Indexing

-- | Retrieve the ASCII character at the given position in the text. Indexes
-- begin from 0. If the index provided is invalid (that is, less than 0, equal
-- to the length of the text, or greater), return 'Nothing'; otherwise, return
-- 'Just' the character at that position.
--
-- >>> index [ascii| "nyan nyan nyan" |] (-100)
-- Nothing
-- >>> index [ascii| "nyan nyan nyan" |] 0
-- Just '0x6e'
-- >>> index [ascii| "nyan nyan nyan" |] 5
-- Just '0x6e'
-- >>> index [ascii| "nyan nyan nyan" |] 2000
-- Nothing
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.1
index :: AsciiText -> Int -> Maybe AsciiChar
index (AT ba off len) i
  | i < 0 = Nothing
  | i >= len = Nothing
  | otherwise = Just . AsciiChar . indexByteArray ba $ (off + i)

-- | Returns 'Just' the first index in the text such that the character at that
-- index satisfies the predicate, 'Nothing' otherwise.
--
-- >>> findIndex ([char| 'n' |] ==) empty
-- Nothing
-- >>> findIndex ([char| 'n' |] ==) . singleton $ [char| 'n' |]
-- Just 0
-- >>> findIndex ([char| 'n' |] ==) . singleton $ [char| 'w' |]
-- Nothing
-- >>> findIndex ([char| 'n' |] ==) [ascii| "nyan" |]
-- Just 0
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
findIndex :: (AsciiChar -> Bool) -> AsciiText -> Maybe Int
findIndex f = F.foldl' go Nothing . P.zip [0 ..] . toList
  where
    go :: Maybe Int -> (Int, AsciiChar) -> Maybe Int
    go acc (i, c) = case acc of
      Nothing -> if f c then Just i else Nothing
      Just _ -> acc

-- | @count needle haystack@, given a @needle@ of length \(n\) and a haystack of
-- length \(h\), counts the number of non-overlapping occurrences of @needle@ in
-- @haystack@. If @needle@ is empty, the count will be 0.
--
-- >>> count empty [ascii| "nyan nyan nyan" |]
-- 0
-- >>> count [ascii| "nyan" |] empty
-- 0
-- >>> count [ascii| "nyan" |] [ascii| "nyan" |]
-- 1
-- >>> count [ascii| "nyan" |] [ascii| "nyan nyan nyan" |]
-- 3
-- >>> count [ascii| "nyan" |] [ascii| "nyanyanyan" |]
-- 2
--
-- = On complexity
--
-- This function is based on a [naive string
-- search](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html),
-- with two added optimizations:
--
-- * We first detect all locations of the first character of the needle; and
-- * Once we find a match, we can skip forward by its length (or more, if the
--   next location is further away).
--
-- The average-case analysis is based on the assumptions that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
--   haystack; and
-- * The needle and haystack together contain at least two unique symbols.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The first symbol of the needle is frequent, but doesn't lead to many
--   matches; or
-- * The needle and/or haystack use few unique symbols.
--
-- Also check the [description of the
-- algorithm](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- for additional pathological case examples.
--
-- /Complexity:/ \(\Theta(h - n)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node3.html)
-- * ["Algorithms on Strings"](https://www.cambridge.org/core/books/algorithms-on-strings/19049704C876795D95D8882C73257C70) by Crochemore, Hancart and Lecroq. PDF available [here](https://www.researchgate.net/publication/220693689_Algorithms_on_Strings).
--
-- @since 1.0.1
count :: AsciiText -> AsciiText -> Int
count (AT needleBa needleOff needleLen) (AT haystackBa haystackOff haystackLen)
  | P.min needleLen haystackLen == 0 = 0
  | otherwise =
    P.length
      . indices needleBa needleOff needleLen haystackBa haystackOff
      $ haystackLen

-- Zipping

-- | \'Pair off\' characters in both texts at corresponding indices. The result
-- will be limited to the shorter of the two arguments.
--
-- >>> zip empty [ascii| "catboy" |]
-- []
-- >>> zip [ascii| "catboy" |] empty
-- []
-- >>> zip [ascii| "catboy" |] [ascii| "nyan" |]
-- [('0x63','0x6e'),('0x61','0x79'),('0x74','0x61'),('0x62','0x6e')]
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
zip :: AsciiText -> AsciiText -> [(AsciiChar, AsciiChar)]
zip at at' = P.zip (toList at) (toList at')

-- | Combine two texts together in lockstep to produce a new text, using the
-- provided function to combine ASCII characters at each step. The length of the
-- result will be the minimum of the lengths of the two text arguments.
--
-- >>> zipWith max [ascii| "I am a catboy" |] empty
-- ""
-- >>> zipWith max empty [ascii| "I am a catboy" |]
-- ""
-- >>> zipWith max [ascii| "I am a catboy" |] [ascii| "Nyan nyan nyan nyan nyan" |]
-- "Nyan nycntnyy"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.1
zipWith ::
  (AsciiChar -> AsciiChar -> AsciiChar) -> AsciiText -> AsciiText -> AsciiText
zipWith f t1 t2 = unfoldr go (t1, t2)
  where
    go :: (AsciiText, AsciiText) -> Maybe (AsciiChar, (AsciiText, AsciiText))
    go (acc1, acc2) = do
      (h1, t1') <- uncons acc1
      (h2, t2') <- uncons acc2
      pure (f h1 h2, (t1', t2'))

-- Decoding

-- | Attempt to decode a string-like type into an ASCII text. Gives @'Left' (i,
-- c)@ on failure, where @i@ is the (zero-based) position of the input where a
-- non-ASCII character first occurred, and @c@ is said character.
--
-- >>> decodeAscii ("catboy" :: String)
-- Right "catboy"
-- >>> decodeAscii ("😺😺😺😺😺" :: String)
-- Left (0, '\128570')
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 2.0.0
decodeAscii :: (IsList s, Item s ~ Char) => s -> Either (Int, Char) AsciiText
decodeAscii = P.fmap fromList . P.traverse go . P.zip [0 ..] . toList
  where
    go :: (Int, Char) -> Either (Int, Char) AsciiChar
    go (i, c)
      | isAscii c = pure . AsciiChar . P.fromIntegral . ord $ c
      | otherwise = Left (i, c)

-- | As 'decodeAscii', but throwing away the error information.
--
-- >>> decodeAsciiMay ("catboy" :: String)
-- Just "catboy"
-- >>> decodeAsciiMay ("😺😺😺😺😺" :: String)
-- Nothing
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 2.0.0
decodeAsciiMay :: (IsList s, Item s ~ Char) => s -> Maybe AsciiText
decodeAsciiMay = P.either (P.const Nothing) Just . decodeAscii

-- | Attempt to decode a bytestring-like type into an ASCII text. Gives @'Left'
-- (i, w8)@ on failure, where @i@ is the (zero-based) position of the input
-- where an out-of-range byte (above 127) first occurred, and @w8@ is said byte.
--
-- >>> decodeBytesAscii ([0x6e, 0x79, 0x61, 0x6e] :: [Word8])
-- Right "nyan"
-- >>> decodeBytesAscii ([0x80] :: [Word8])
-- Left (0,128)
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 2.0.0
decodeBytesAscii :: (IsList s, Item s ~ Word8) => s -> Either (Int, Word8) AsciiText
decodeBytesAscii = P.fmap fromList . P.traverse go . P.zip [0 ..] . toList
  where
    go :: (Int, Word8) -> Either (Int, Word8) AsciiChar
    go (i, w8)
      | w8 <= 127 = pure . AsciiChar $ w8
      | otherwise = Left (i, w8)

-- | As 'decodeBytesAscii', but throwing away the error information.
--
-- >>> decodeBytesAscii ([0x6e, 0x79, 0x61, 0x6e] :: [Word8])
-- Just "nyan"
-- >>> decodeBytesAscii ([0x80] :: [Word8])
-- Nothing
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 2.0.0
decodeBytesAsciiMay :: (IsList s, Item s ~ Word8) => s -> Maybe AsciiText
decodeBytesAsciiMay = P.either (P.const Nothing) Just . decodeBytesAscii

-- Encoding

-- | Encode an ASCII text into a string-like type.
--
-- >>> encodeAscii empty :: String
-- ""
-- >>> encodeAscii . singleton $ [char| 'w' |] :: String
-- "w"
-- >>> encodeAscii [ascii| "nyan" |] :: String
-- "nyan"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 2.0.0
encodeAscii :: (IsList s, Item s ~ Char) => AsciiText -> s
encodeAscii =
  fromList
    . P.fmap (chr . P.fromIntegral . coerce @AsciiChar @Word8)
    . toList

-- Conversion

-- | Convert an ASCII text into a sequence of bytes.
--
-- >>> toBytes empty :: [Word8]
-- []
-- >>> toBytes . singleton $ [char| 'w' |] :: [Word8]
-- []
-- >>> toBytes [ascii| "nyan"] :: [Word8]
-- []
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 2.0.0
toBytes :: (IsList s, Item s ~ Word8) => AsciiText -> s
toBytes = fromList . P.fmap coerce . toList

-- TODO: Doctests for unpackAscii, packAscii

-- | \'Unpack\' ASCII text into another linear structure.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 2.0.0
unpackAscii :: (IsList s, Item s ~ AsciiChar) => AsciiText -> s
unpackAscii at = fromListN (length at) . toList $ at

-- | \'Pack\' ASCII text data from a linear structure to an 'AsciiText'.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 2.0.0
packAscii :: (IsList s, Item s ~ AsciiChar) => s -> AsciiText
packAscii = fromList . toList

-- Optics

-- | A demonstration of the relationship between 'decodeAsciiMay' and
-- 'encodeAscii'.
--
-- >>> preview _AsAscii ("catboy goes nyan" :: String)
-- Just "catboy goes nyan"
-- >>> preview _AsAscii "😺😺😺😺😺"
-- Nothing
-- >>> review _AsAscii [ascii| "catboys are amazing" |] :: String
-- "catboys are amazing"
--
-- @since 2.0.0
_AsAscii :: (IsList s, Item s ~ Char) => Prism' s AsciiText
_AsAscii = prism' encodeAscii decodeAsciiMay

-- | A demonstration of the relationship between 'decodeBytesAsciiMay' and
-- 'toBytes'.
--
-- >>> preview _AsBytesAscii ([0x6e, 0x79, 0x61, 0x6e] :: [Word8])
-- Just "nyan"
-- >>> preview _AsBytesAscii ([0xff, 0xff] :: [Word8])
-- Nothing
-- >>> review _AsBytesAscii [ascii| "nyan" |]
-- []
--
-- @since 2.0.0
_AsBytesAscii :: (IsList s, Item s ~ Word8) => Prism' s AsciiText
_AsBytesAscii = prism' toBytes decodeBytesAsciiMay

-- | Pack (or unpack) a linear structure of ASCII characters into an
-- 'AsciiText'.
--
-- >>> view _PackedAscii [[char| 'n' |], [char| 'y' |], [char| 'a' |], [char| 'n' |]]
-- "nyan"
-- >>> review _PackedAscii [ascii| "nyan" |]
-- ['0x6e','0x79','0x61','0x6e']
--
-- @since 2.0.0
_PackedAscii :: (IsList s, Item s ~ AsciiChar) => Iso' s AsciiText
_PackedAscii = iso packAscii unpackAscii

-- | Traverse the individual ASCII characters in a text.
--
-- >>> preview (elementOf chars 0) [ascii| "I am a catboy" |]
-- Just '0x49'
-- >>> preview (elementOf chars 100) [ascii| "I am a catboy" |]
-- Nothing
-- >>> iover chars (\i x -> bool x [char| 'w' |] . even $ i) [ascii| "I am a catboy" |]
-- "w wmwawcwtwow"
--
-- @since 2.0.0
chars :: IxTraversal' Int AsciiText AsciiChar
chars = itraversalVL (\f at -> P.fmap fromList . itraverse f . toList $ at)

-- | Access the individual bytes in a text. This isn't as capable as 'chars', as
-- that would allow modifications of the bytes in ways that aren't valid as
-- ASCII.
--
-- >>> itoListOf bytes [ascii| "I am a catboy" |]
-- [(0,73),(1,32),(2,97),(3,109),(4,32),(5,97),(6,32),(7,99),(8,97),(9,116),(10,98),(11,111),(12,121)]
--
-- @since 2.0.0
bytes :: IxFold Int AsciiText Word8
bytes = ifoldVL (\f at -> itraverse_ f . coerce @[AsciiChar] @[Word8] . toList $ at)

-- Low-level

-- | Make a distinct copy of the argument, such that the copy shares no memory
-- with it. Other than the copying, identical to `P.id`.
--
-- This function is useful if you only need a small portion of a much larger
-- string; this way, you can release all the memory associated with the much
-- larger string immediately, rather than waiting until the small portion is no
-- longer needed.
--
-- >>> copy [ascii| "I am a catboy." |]
-- " I am a catboy."
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 2.0.0
copy :: AsciiText -> AsciiText
copy (AT ba off len) = runST $ do
  mba <- newByteArray len
  copyByteArray mba 0 ba off len
  frozen <- unsafeFreezeByteArray mba
  pure . AT frozen 0 $ len

-- Helpers

isSpace :: AsciiChar -> Bool
isSpace (AsciiChar w8)
  | w8 == 32 = True
  | 9 <= w8 && w8 <= 13 = True
  | otherwise = False

indices :: ByteArray -> Int -> Int -> ByteArray -> Int -> Int -> [Int]
indices nba noff nlen hba@(ByteArray hba#) hoff hlen
  | nlen == 0 = []
  | nlen > hlen = []
  | nlen == 1 = do
    let w8 :: Word8 = indexByteArray nba noff
    let d :: Int = hlen `P.quot` 8
    L.concatMap (wordStep1 w8) [hoff, hoff + 8 .. hoff + (8 * d - 1)]
      <> byteStep1 w8 (hoff + 8 * d)
  | otherwise =
    L.concatMap wordStep [hoff, hoff + 8 .. hoff + hlen - nlen + 1]
      <> byteStep (hoff + hlen - nlen + 1) (hoff + 8 * ((hlen - nlen) `P.quot` 8))
  where
    blockFirst :: Word64
    blockFirst = broadcast first
    first :: Word8
    first = indexByteArray nba noff
    blockLast :: Word64
    blockLast = broadcast . indexByteArray nba $ noff + nlen - 1
    wordStep1 :: Word8 -> Int -> [Int]
    wordStep1 w8 wordI@(I# wordI#) = do
      let w :: Word64 = W64# (indexWord8ArrayAsWord64# hba# wordI#)
      let input :: Word64 = w `xor` broadcast w8
      let final :: Word64 = complement ((input + loOrderMask) .|. loOrderMask)
      case popCount final of
        0 -> []
        1 -> [wordI + (countTrailingZeros final `P.quot` 8)]
        2 -> P.fmap ((wordI +) . select final) [0, 1]
        3 -> P.fmap ((wordI +) . select final) [0 .. 2]
        4 -> P.fmap ((wordI +) . select final) [0 .. 3]
        5 -> P.fmap ((wordI +) . select final) [0 .. 4]
        6 -> P.fmap ((wordI +) . select final) [0 .. 5]
        7 -> P.fmap ((wordI +) . select final) [0 .. 6]
        _ -> [wordI .. wordI + 7]
    wordStep :: Int -> [Int]
    wordStep wordI@(I# wordI#) = do
      let w :: Word64 = W64# (indexWord8ArrayAsWord64# hba# wordI#)
      let !(I# jumpI#) = wordI + nlen - 1
      let w' :: Word64 = W64# (indexWord8ArrayAsWord64# hba# jumpI#)
      let input :: Word64 = (w `xor` blockFirst) .|. (w' `xor` blockLast)
      let final :: Word64 = complement ((input + loOrderMask) .|. loOrderMask)
      case popCount final of
        0 -> []
        1 -> do
          let off = countTrailingZeros final `P.quot` 8
          case compareByteArrays hba (wordI + off) nba noff nlen of
            P.EQ -> [wordI + off]
            _ -> []
        pop -> do
          let selected = P.fmap (select final) [0 .. pop - 1]
          selectGo wordI final selected
    selectGo :: Int -> Word64 -> [Int] -> [Int]
    selectGo wordI final = \case
      [] -> []
      (sI : sIs) -> case compareByteArrays hba (wordI + sI) nba noff nlen of
        P.EQ -> wordI + sI : (selectGo wordI final . skip sI $ sIs)
        _ -> selectGo wordI final sIs
    byteStep1 :: Word8 -> Int -> [Int]
    byteStep1 w8 byteI
      | byteI == hlen = []
      | indexByteArray hba byteI == w8 = byteI : byteStep1 w8 (byteI + 1)
      | otherwise = byteStep1 w8 (byteI + 1)
    byteStep :: Int -> Int -> [Int]
    byteStep lim byteI
      | byteI > lim = []
      | indexByteArray hba byteI /= first = byteStep lim (byteI + 1)
      | otherwise = case compareByteArrays hba byteI nba noff nlen of
        P.EQ -> byteI : byteStep lim (byteI + nlen)
        _ -> byteStep lim (byteI + 1)
    loOrderMask :: Word64
    loOrderMask = 0x7F7F7F7F7F7F7F7F
    skip :: Int -> [Int] -> [Int]
    skip i = P.dropWhile (\j -> j - i < nlen)

broadcast :: Word8 -> Word64
broadcast w8 = P.fromIntegral w8 * (0x0101010101010101 :: Word64)

select :: Word64 -> Int -> Int
select (W64# mask) i =
  let !(W64# src) = bit i
      res = W64# (ctz64# (pdep64# src mask))
   in P.fromIntegral (res `shiftR` 3)
