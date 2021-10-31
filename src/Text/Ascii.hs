{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
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
    commonPrefixes,

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

    -- * Conversions
    fromText,
    eitherFromText,
    fromByteString,
    eitherFromByteString,
    toText,
    toByteString,

    -- * Optics
    textWise,
    byteStringWise,
    packedChars,
    chars,
    packedBytes,
    bytes,
  )
where

import Control.Category ((.))
import Data.Bifunctor (first)
import Data.Bool (Bool (False, True), otherwise, (&&))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Optics as BSO
import Data.Char (isAscii)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (foldMap))
import qualified Data.Foldable as F
import Data.Int (Int64)
import qualified Data.List as L
import Data.Maybe (Maybe (Just, Nothing))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word8)
import Optics.Coerce (coerceA, coerceB, coerceS, coerceT)
import Optics.Getter (Getter, view)
import Optics.Iso (Iso')
import Optics.IxFold (IxFold)
import Optics.IxTraversal (IxTraversal')
import Optics.Optic (castOptic)
import Optics.Prism (Prism', prism')
import Optics.Review (Review, review)
import Text.Ascii.Internal (AsciiChar (AsciiChar), AsciiText (AsciiText))
import Text.Ascii.QQ (ascii, char)
import Prelude
  ( Int,
    not,
    pure,
    ($),
    (+),
    (-),
    (/=),
    (<),
    (<$>),
    (<=),
    (<>),
    (==),
    (>),
    (>=),
    (||),
  )
import qualified Prelude as P

-- Note on pragmata
--
-- This is cribbed directly from bytestring, as I figure they know what they're
-- doing way better than we do. When we add our own functionality, this probably
-- needs to be considered more carefully. - Koz

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
empty = coerce BS.empty

-- | A text consisting of a single ASCII character.
--
-- >>> singleton [char| 'w' |]
-- "w"
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
{-# INLINE [1] singleton #-}
singleton :: AsciiChar -> AsciiText
singleton = coerce BS.singleton

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
{-# INLINE cons #-}
cons :: AsciiChar -> AsciiText -> AsciiText
cons = coerce BS.cons

-- | Adds a character to the back of a text. This requires copying, which gives
-- its complexity.
--
-- >>> snoc [ascii| "nek" |] [char| 'o' |]
-- "neko"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE snoc #-}
snoc :: AsciiText -> AsciiChar -> AsciiText
snoc = coerce BS.snoc

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
{-# INLINE uncons #-}
uncons :: AsciiText -> Maybe (AsciiChar, AsciiText)
uncons = coerce BS.uncons

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
{-# INLINE unsnoc #-}
unsnoc :: AsciiText -> Maybe (AsciiText, AsciiChar)
unsnoc = coerce BS.unsnoc

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
{-# INLINE length #-}
length :: AsciiText -> Int
length = coerce BS.length

-- Transformations

-- | Copy, and apply the function to each element of, the text.
--
-- >>> map (\c -> fromMaybe c . upcase $ c) [ascii| "nyan!" |]
-- "NYAN!"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE map #-}
map :: (AsciiChar -> AsciiChar) -> AsciiText -> AsciiText
map = coerce BS.map

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
{-# INLINE [1] intercalate #-}
intercalate :: AsciiText -> [AsciiText] -> AsciiText
intercalate = coerce BS.intercalate

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
intersperse = coerce BS.intersperse

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
transpose = coerce BS.transpose

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
reverse = coerce BS.reverse

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
-- This function is based on a variant of the
-- [NSN](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html) algorithm,
-- except it does not detect overlapping needles. Its average-case analysis is
-- based on the assumption that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
-- haystack; and
-- * The needle has length at least two; and
-- * Both the needle and the haystack contain at least four unique symbols.
--
-- We fall back to 'split' for singleton needles, and there is no work to be
-- done on empty needles, which means the second assumption always holds.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The needle and/or haystack use few unique symbols (less than four is the
-- worst); or
-- * The haystack contains many instances of the second symbol of the needle
-- which don't lead to full matches.
--
-- The analysis below also doesn't factor in the cost of performing the
-- replacement, as this is (among other things) proportional to the number of
-- matches of the needle (and thus is hard to quantify).
--
-- /Complexity:/ \(\Theta(h)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html)
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
{-# INLINE foldl #-}
foldl :: (a -> AsciiChar -> a) -> a -> AsciiText -> a
foldl f x (AsciiText bs) = BS.foldl (coerce f) x bs

-- | Left-associative fold of a text, strict in the accumulator.
--
-- >>> foldl' (\acc c -> [ascii| "f(" |] <> acc <> singleton c <> [ascii| ")" |]) [ascii| "a" |] [ascii| "catboy" |]
-- "f(f(f(f(f(f(ac)a)t)b)o)y)"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE foldl' #-}
foldl' :: (a -> AsciiChar -> a) -> a -> AsciiText -> a
foldl' f x (AsciiText bs) = BS.foldl' (coerce f) x bs

-- | Right-associative fold of a text.
--
-- >>> foldr (\c acc -> [ascii| "f(" |] <> acc <> singleton c <> [ascii| ")" |]) [ascii| "a" |] [ascii| "catboy" |]
-- "f(f(f(f(f(f(ay)o)b)t)a)c)"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE foldr #-}
foldr :: (AsciiChar -> a -> a) -> a -> AsciiText -> a
foldr f x (AsciiText bs) = BS.foldr (coerce f) x bs

-- | Right-associative fold of a text, strict in the accumulator.
--
-- >>> foldr' (\c acc -> [ascii| "f(" |] <> acc <> singleton c <> [ascii| ")" |]) [ascii| "a" |] [ascii| "catboy" |]
-- "f(f(f(f(f(f(ay)o)b)t)a)c)"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE foldr' #-}
foldr' :: (AsciiChar -> a -> a) -> a -> AsciiText -> a
foldr' f x (AsciiText bs) = BS.foldr' (coerce f) x bs

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
concat = coerce BS.concat

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
concatMap = coerce BS.concatMap

-- | 'scanl' is similar to 'foldl', but returns a list of successive values from
-- the left.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE scanl #-}
scanl ::
  -- | accumulator -> element -> new accumulator
  (AsciiChar -> AsciiChar -> AsciiChar) ->
  -- | Starting accumulator value
  AsciiChar ->
  -- | Input of length \(n\)
  AsciiText ->
  -- | Output of length \(n + 1\)
  AsciiText
scanl = coerce BS.scanl

-- | 'scanr' is similar to 'foldr', but returns a list of successive values from
-- the right.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE scanr #-}
scanr ::
  -- | element -> accumulator -> new accumulator
  (AsciiChar -> AsciiChar -> AsciiChar) ->
  -- | Starting accumulator value
  AsciiChar ->
  -- | Input of length \(n\)
  AsciiText ->
  -- | Output of length \(n + 1\)
  AsciiText
scanr = coerce BS.scanr

-- Accumulating maps

-- | Like a combination of 'map' and 'foldl''. Applies a function to each
-- element of an 'AsciiText', passing an accumulating parameter from left to
-- right, and returns a final 'AsciiText' along with the accumulating
-- parameter's final value.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE mapAccumL #-}
mapAccumL :: (a -> AsciiChar -> (a, AsciiChar)) -> a -> AsciiText -> (a, AsciiText)
mapAccumL f x (AsciiText bs) = AsciiText <$> BS.mapAccumL (coerce f) x bs

-- | Like a combination of 'map' and 'foldr'. Applies a function to each element
-- of an 'AsciiText', passing an accumulating parameter from right to left, and
-- returns a final 'AsciiText' along with the accumulating parameter's final
-- value.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE mapAccumR #-}
mapAccumR :: (a -> AsciiChar -> (a, AsciiChar)) -> a -> AsciiText -> (a, AsciiText)
mapAccumR f x (AsciiText bs) = AsciiText <$> BS.mapAccumL (coerce f) x bs

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
  | otherwise = concat . P.replicate n $ t

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
{-# INLINE unfoldr #-}
unfoldr :: (a -> Maybe (AsciiChar, a)) -> a -> AsciiText
unfoldr f = AsciiText . BS.unfoldr (coerce f)

-- | Similar to 'unfoldr', but also takes a maximum length parameter. The second
-- element of the result tuple will be 'Nothing' if we finished with the
-- function argument returning 'Nothing', and 'Just' the final seed value if we
-- reached the maximum length before that happened.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE unfoldrN #-}
unfoldrN :: Int -> (a -> Maybe (AsciiChar, a)) -> a -> (AsciiText, Maybe a)
unfoldrN n f = first AsciiText . BS.unfoldrN n (coerce f)

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
{-# INLINE take #-}
take :: Int -> AsciiText -> AsciiText
take = coerce BS.take

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
{-# INLINE drop #-}
drop :: Int -> AsciiText -> AsciiText
drop = coerce BS.drop

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
{-# INLINE [1] takeWhile #-}
takeWhile :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
takeWhile f (AsciiText at) = AsciiText . BS.takeWhile (coerce f) $ at

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
{-# INLINE takeWhileEnd #-}
takeWhileEnd :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
takeWhileEnd f = AsciiText . BS.takeWhileEnd (coerce f) . coerce

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
{-# INLINE [1] dropWhile #-}
dropWhile :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
dropWhile f (AsciiText at) = AsciiText . BS.dropWhile (coerce f) $ at

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
{-# INLINE dropWhileEnd #-}
dropWhileEnd :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
dropWhileEnd f = AsciiText . BS.dropWhileEnd (coerce f) . coerce

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
{-# INLINE splitAt #-}
splitAt :: Int -> AsciiText -> (AsciiText, AsciiText)
splitAt = coerce BS.splitAt

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
-- This function is based on a variant of the
-- [NSN](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html) algorithm,
-- except it does not detect overlapping needles. Its average-case analysis is
-- based on the assumption that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
-- haystack; and
-- * The needle has length at least two; and
-- * Both the needle and the haystack contain at least four unique symbols.
--
-- We fall back to 'split' for singleton needles, and there is no work to be
-- done on empty needles, which means the second assumption always holds.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The needle and/or haystack use few unique symbols (less than four is the
-- worst); or
-- * The haystack contains many instances of the second symbol of the needle
-- which don't lead to full matches.
--
-- /Complexity:/ \(\Theta(h)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html)
-- * ["Algorithms on Strings"](https://www.cambridge.org/core/books/algorithms-on-strings/19049704C876795D95D8882C73257C70) by Crochemore, Hancart and Lecroq. PDF available [here](https://www.researchgate.net/publication/220693689_Algorithms_on_Strings).
--
-- @since 1.0.1
breakOn :: AsciiText -> AsciiText -> (AsciiText, AsciiText)
breakOn needle@(AsciiText n) haystack@(AsciiText h)
  | length needle == 0 = (empty, haystack)
  | otherwise = case indices n h of
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
-- This function is based on a variant of the
-- [NSN](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html) algorithm,
-- except it does not detect overlapping needles. Its average-case analysis is
-- based on the assumption that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
-- haystack; and
-- * The needle has length at least two; and
-- * Both the needle and the haystack contain at least four unique symbols.
--
-- We fall back to 'split' for singleton needles, and there is no work to be
-- done on empty needles, which means the second assumption always holds.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The needle and/or haystack use few unique symbols (less than four is the
-- worst); or
-- * The haystack contains many instances of the second symbol of the needle
-- which don't lead to full matches.
--
-- /Complexity:/ \(\Theta(h)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html)
-- * ["Algorithms on Strings"](https://www.cambridge.org/core/books/algorithms-on-strings/19049704C876795D95D8882C73257C70) by Crochemore, Hancart and Lecroq. PDF available [here](https://www.researchgate.net/publication/220693689_Algorithms_on_Strings).
--
-- @since 1.0.1
breakOnEnd :: AsciiText -> AsciiText -> (AsciiText, AsciiText)
breakOnEnd needle@(AsciiText n) haystack@(AsciiText h)
  | length needle == 0 = (haystack, empty)
  | otherwise = case go . indices n $ h of
    Nothing -> (empty, haystack)
    Just ix -> splitAt (ix + length needle) haystack
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
break = coerce BS.break

-- | @span p t@ is equivalent to @('takeWhile' p t, 'dropWhile' p t)@.
--
-- >>> span ([char| 'c' |] ==) [ascii| "catboy goes nyan" |]
-- ("c","atboy goes nyan")
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE [1] span #-}
span :: (AsciiChar -> Bool) -> AsciiText -> (AsciiText, AsciiText)
span = coerce BS.span

-- | Separate a text into a list of texts such that:
--
-- * Their concatenation is equal to the original argument; and
-- * Equal adjacent characters in the original argument are in the same text in
-- the result.
--
-- This is a specialized form of 'groupBy', and is about 40% faster than
-- @'groupBy' '=='@.
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
group = coerce BS.group

-- | Separate a text into a list of texts such that:
--
-- * Their concatenation is equal to the original argument; and
-- * Adjacent characters for which the function argument returns @True@ are in
-- the same text in the result.
--
-- 'group' is a special case for the function argument '=='; it is also about
-- 40% faster.
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
groupBy = coerce BS.groupBy

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
inits = coerce BS.inits

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
tails = coerce BS.tails

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
-- This function is based on a variant of the
-- [NSN](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html) algorithm,
-- except it does not detect overlapping needles. Its average-case analysis is
-- based on the assumption that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
-- haystack; and
-- * The needle has length at least two; and
-- * Both the needle and the haystack contain at least four unique symbols.
--
-- We fall back to 'split' for singleton needles, and there is no work to be
-- done on empty needles, which means the second assumption always holds.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The needle and/or haystack use few unique symbols (less than four is the
-- worst); or
-- * The haystack contains many instances of the second symbol of the needle
-- which don't lead to full matches.
--
-- /Complexity:/ \(\Theta(h)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html)
-- * ["Algorithms on Strings"](https://www.cambridge.org/core/books/algorithms-on-strings/19049704C876795D95D8882C73257C70) by Crochemore, Hancart and Lecroq. PDF available [here](https://www.researchgate.net/publication/220693689_Algorithms_on_Strings).
--
-- @since 1.0.1
splitOn :: AsciiText -> AsciiText -> [AsciiText]
splitOn needle@(AsciiText n) haystack@(AsciiText h)
  | needleLen == 0 = [haystack]
  | length haystack == 0 = [empty]
  | needleLen == 1 = split (== (AsciiChar . BS.head $ n)) haystack
  | otherwise = go 0 (indices n h)
  where
    needleLen :: Int
    needleLen = length needle
    go :: Int -> [Int] -> [AsciiText]
    go pos = \case
      [] -> [drop pos haystack]
      (ix : ixes) ->
        let chunkLen = ix - pos
            segment = take chunkLen . drop pos $ haystack
         in segment : go (pos + chunkLen + needleLen) ixes

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
{-# INLINE split #-}
split :: (AsciiChar -> Bool) -> AsciiText -> [AsciiText]
split = coerce BS.splitWith

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
lines (AsciiText bs) = coerce . go $ bs
  where
    go :: ByteString -> [ByteString]
    go rest = case BS.uncons rest of
      Nothing -> []
      Just _ -> case BS.break (0x0a ==) rest of
        (h, t) ->
          h : case BS.uncons t of
            Nothing -> []
            Just (_, t') -> go t'

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
stripPrefix = coerce BS.stripPrefix

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
stripSuffix = coerce BS.stripSuffix

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
-- This function is based on a variant of the
-- [NSN](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html) algorithm,
-- except it does not detect overlapping needles. Its average-case analysis is
-- based on the assumption that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
-- haystack; and
-- * The needle has length at least two; and
-- * Both the needle and the haystack contain at least four unique symbols.
--
-- We fall back to 'split' for singleton needles, and there is no work to be
-- done on empty needles, which means the second assumption always holds.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The needle and/or haystack use few unique symbols (less than four is the
-- worst); or
-- * The haystack contains many instances of the second symbol of the needle
-- which don't lead to full matches.
--
-- /Complexity:/ \(\Theta(h)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html)
-- * ["Algorithms on Strings"](https://www.cambridge.org/core/books/algorithms-on-strings/19049704C876795D95D8882C73257C70) by Crochemore, Hancart and Lecroq. PDF available [here](https://www.researchgate.net/publication/220693689_Algorithms_on_Strings).
--
-- @since 1.0.1
stripInfix :: AsciiText -> AsciiText -> Maybe (AsciiText, AsciiText)
stripInfix needle@(AsciiText n) haystack@(AsciiText h)
  | P.min (length needle) (length haystack) == 0 = Nothing
  | otherwise = case indices n h of
    [] -> Nothing
    (ix : _) -> Just (take ix haystack, drop (ix + length needle) haystack)

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
{-# INLINE filter #-}
filter :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
filter = coerce BS.filter

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
-- This function is based on a variant of the
-- [NSN](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html) algorithm,
-- except it does not detect overlapping needles. Its average-case analysis is
-- based on the assumption that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
-- haystack; and
-- * The needle has length at least two; and
-- * Both the needle and the haystack contain at least four unique symbols.
--
-- We fall back to 'split' for singleton needles, and there is no work to be
-- done on empty needles, which means the second assumption always holds.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The needle and/or haystack use few unique symbols (less than four is the
-- worst); or
-- * The haystack contains many instances of the second symbol of the needle
-- which don't lead to full matches.
--
-- /Complexity:/ \(\Theta(h)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html)
-- * ["Algorithms on Strings"](https://www.cambridge.org/core/books/algorithms-on-strings/19049704C876795D95D8882C73257C70) by Crochemore, Hancart and Lecroq. PDF available [here](https://www.researchgate.net/publication/220693689_Algorithms_on_Strings).
--
-- @since 1.0.1
breakOnAll :: AsciiText -> AsciiText -> [(AsciiText, AsciiText)]
breakOnAll needle@(AsciiText n) haystack@(AsciiText h)
  | length needle == 0 = [(haystack, empty)]
  | length haystack == 0 = []
  | otherwise = (`splitAt` haystack) <$> indices n h

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
{-# INLINE find #-}
find :: (AsciiChar -> Bool) -> AsciiText -> Maybe AsciiChar
find = coerce BS.find

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
partition = coerce BS.partition

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
index at i
  | i < 0 || i >= length at = Nothing
  | otherwise = Just . coerce BS.index at $ i

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
{-# INLINE [1] findIndex #-}
findIndex :: (AsciiChar -> Bool) -> AsciiText -> Maybe Int
findIndex = coerce BS.findIndex

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
-- This function is based on a variant of the
-- [NSN](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html) algorithm,
-- except it does not detect overlapping needles. Its average-case analysis is
-- based on the assumption that:
--
-- * All ASCII symbols are equally likely to occur in both the needle and the
-- haystack; and
-- * The needle has length at least two; and
-- * Both the needle and the haystack contain at least four unique symbols.
--
-- We fall back to 'split' for singleton needles, and there is no work to be
-- done on empty needles, which means the second assumption always holds.
--
-- Worst-case behaviour becomes more likely the more your input satisfies the
-- following conditions:
--
-- * The needle and/or haystack use few unique symbols (less than four is the
-- worst); or
-- * The haystack contains many instances of the second symbol of the needle
-- which don't lead to full matches.
--
-- /Complexity:/ \(\Theta(h)\) average case, \(\Theta(h \cdot n\)\) worst-case.
--
-- /See also:/ Note that all the below are references for the original
-- algorithm, which includes searching for overlapping needles. Thus, our
-- implementation will perform better than the analysis suggests.
--
-- * [Description and pseudocode](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html)
-- * ["Algorithms on Strings"](https://www.cambridge.org/core/books/algorithms-on-strings/19049704C876795D95D8882C73257C70) by Crochemore, Hancart and Lecroq. PDF available [here](https://www.researchgate.net/publication/220693689_Algorithms_on_Strings).
--
-- @since 1.0.1
count :: AsciiText -> AsciiText -> Int
count needle@(AsciiText n) haystack@(AsciiText h)
  | P.min (length needle) (length haystack) == 0 = 0
  | length needle == 1 = BS.count (BS.head n) h
  | otherwise = P.length . indices n $ h

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
zip = coerce BS.zip

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

-- Conversions

-- | Try and convert a 'Text' into an 'AsciiText'. Gives 'Nothing' if the 'Text'
-- contains any symbols which lack an ASCII equivalent.
--
-- >>> fromText "catboy"
-- Just "catboy"
-- >>> fromText "😺😺😺😺😺"
-- Nothing
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
fromText :: Text -> Maybe AsciiText
fromText t = case T.find (not . isAscii) t of
  Nothing -> pure . AsciiText . encodeUtf8 $ t
  Just _ -> Nothing

-- | Try and convert a 'Text' into an 'AsciiText'. Gives @'Prelude.Left' c@ if the 'Text'
-- contains a 'Prelude.Char' @c@ that lacks an ASCII representation.
--
-- >>> eitherFromText "catboy"
-- Right "catboy"
-- >>> eitherFromText "😺😺😺😺😺"
-- Left '\128570'
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.1
eitherFromText :: Text -> P.Either P.Char AsciiText
eitherFromText t = case T.find (not . isAscii) t of
  Nothing -> pure . AsciiText . encodeUtf8 $ t
  Just c -> P.Left c

-- | Try and convert a 'ByteString' into an 'AsciiText'. Gives 'Nothing' if the
-- 'ByteString' contains any bytes outside the ASCII range (that is, from 0 to
-- 127 inclusive).
--
-- >>> fromByteString "catboy"
-- Just "catboy"
-- >>> fromByteString . BS.pack $ [128]
-- Nothing
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
fromByteString :: ByteString -> Maybe AsciiText
fromByteString bs = case BS.find (> 127) bs of
  Nothing -> pure . AsciiText $ bs
  Just _ -> Nothing

-- | Try and convert a 'ByteString' into an 'AsciiText'. Gives @'Prelude.Left' w8@ if
-- the 'ByteString' contains a byte @w8@ that is outside the ASCII range (that
-- is, from 0 to 127 inclusive).
--
-- >>> eitherFromByteString "catboy"
-- Right "catboy"
-- >>> eitherFromByteString . BS.pack $ [128]
-- Left 128
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.1
eitherFromByteString :: ByteString -> P.Either Word8 AsciiText
eitherFromByteString bs = case BS.find (> 127) bs of
  Nothing -> pure . AsciiText $ bs
  Just w8 -> P.Left w8

-- | Convert an 'AsciiText' into a 'Text' (by copying).
--
-- >>> toText empty
-- ""
-- >>> toText . singleton $ [char| 'w' |]
-- "w"
-- >>> toText [ascii| "nyan" |]
-- "nyan"
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
toText :: AsciiText -> Text
toText (AsciiText bs) = decodeUtf8 bs

-- | Reinterpret an 'AsciiText' as a 'ByteString' (without copying).
--
-- >>> toByteString empty
-- ""
-- >>> toByteString . singleton $ [char| 'w' |]
-- "w"
-- >>> toByteString [ascii| "nyan" |]
-- "nyan"
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
toByteString :: AsciiText -> ByteString
toByteString = coerce

-- Optics

-- | A convenient demonstration of the relationship between 'toText' and
-- 'fromText'.
--
-- >>> preview textWise "catboy goes nyan"
-- Just "catboy goes nyan"
-- >>> preview textWise "😺😺😺😺😺"
-- Nothing
-- >>> review textWise [ascii| "catboys are amazing" |]
-- "catboys are amazing"
--
-- @since 1.0.0
textWise :: Prism' Text AsciiText
textWise = prism' toText fromText

-- | A convenient demonstration of the relationship between 'toByteString' and
-- 'fromByteString'.
--
-- >>> preview byteStringWise "catboy goes nyan"
-- Just "catboy goes nyan"
-- >>> preview byteStringWise . BS.pack $ [0xff, 0xff]
-- Nothing
-- >>> review byteStringWise [ascii| "I love catboys" |]
-- "I love catboys"
--
-- @since 1.0.0
byteStringWise :: Prism' ByteString AsciiText
byteStringWise = prism' toByteString fromByteString

-- | Pack (or unpack) a list of ASCII characters into a text.
--
-- >>> view packedChars [[char| 'n' |], [char| 'y' |], [char| 'a' |], [char| 'n' |]]
-- "nyan"
-- >>> review packedChars [ascii| "nyan" |]
-- ['0x6e','0x79','0x61','0x6e']
--
-- @since 1.0.1
packedChars :: Iso' [AsciiChar] AsciiText
packedChars =
  coerceS . coerceT . coerceA . coerceB $ BSO.packedBytes @ByteString

-- | Traverse the individual ASCII characters in a text.
--
-- >>> preview (elementOf chars 0) [ascii| "I am a catboy" |]
-- Just '0x49'
-- >>> preview (elementOf chars 100) [ascii| "I am a catboy" |]
-- Nothing
-- >>> iover chars (\i x -> bool x [char| 'w' |] . even $ i) [ascii| "I am a catboy" |]
-- "w wmwawcwtwow"
--
-- @since 1.0.1
chars :: IxTraversal' Int64 AsciiText AsciiChar
chars = coerceS . coerceT . coerceA . coerceB $ BSO.bytes @ByteString

-- | Pack (or unpack) a list of bytes into a text. This isn't as capable as
-- 'packedChars', as that would allow construction of invalid texts.
--
-- >>> preview packedBytes [0x6e, 0x79, 0x61, 0x6e]
-- Just "nyan"
-- >>> preview packedBytes [0xff, 0xfe]
-- Nothing
-- >>> review packedBytes [ascii| "nyan" |]
-- [110,121,97,110]
--
-- @since 1.0.1
packedBytes :: Prism' [Word8] AsciiText
packedBytes = prism' (review go) (P.fmap (view go2) . P.traverse asciify)
  where
    go :: Review [Word8] AsciiText
    go = castOptic . coerceA . coerceB $ BSO.packedBytes @ByteString
    go2 :: Getter [Word8] AsciiText
    go2 = castOptic . coerceA . coerceB $ BSO.packedBytes @ByteString

-- | Access the individual bytes in a text. This isn't as capable as 'chars', as
-- that would allow modifications of the bytes in ways that aren't valid as
-- ASCII.
--
-- >>> itoListOf bytes [ascii| "I am a catboy" |]
-- [(0,73),(1,32),(2,97),(3,109),(4,32),(5,97),(6,32),(7,99),(8,97),(9,116),(10,98),(11,111),(12,121)]
--
-- @since 1.0.1
bytes :: IxFold Int64 AsciiText Word8
bytes = castOptic . coerceS . coerceT $ BSO.bytes @ByteString

-- Helpers

isSpace :: AsciiChar -> Bool
isSpace (AsciiChar w8)
  | w8 == 32 = True
  | 9 <= w8 && w8 <= 13 = True
  | otherwise = False

asciify :: Word8 -> Maybe Word8
asciify w8
  | w8 <= 127 = Just w8
  | otherwise = Nothing

indices :: ByteString -> ByteString -> [Int]
indices needle haystack
  | P.min needleLen haystackLen == 0 = []
  | needleLen == 1 = BS.elemIndices (BS.head needle) haystack
  | otherwise = L.unfoldr go 0
  where
    go :: Int -> Maybe (Int, Int)
    go j
      | j > (haystackLen - needleLen) = Nothing
      | BS.index needle 1 /= BS.index haystack (j + 1) = go (j + kay)
      | otherwise = do
        let fragment = BS.take needleLen . BS.drop j $ haystack
        if fragment == needle
          then pure (j, j + needleLen)
          else go (j + ell)
    kay :: Int
    kay
      | BS.head needle == BS.index needle 1 = 2
      | otherwise = 1
    ell :: Int
    ell
      | BS.head needle == BS.index needle 1 = 1
      | otherwise = 2
    needleLen :: Int
    needleLen = BS.length needle
    haystackLen :: Int
    haystackLen = BS.length haystack
