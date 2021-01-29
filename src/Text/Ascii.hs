{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Ascii
  ( -- * Type
    AsciiText,

    -- * Creation
    empty,
    singleton,

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
    unfoldr,

    -- * Substrings

    -- ** Breaking strings
    take,
    drop,
    takeWhile,
    dropWhile,
    splitAt,
    break,
    span,
    group,
    groupBy,
    inits,
    tails,

    -- ** Breaking into many substrings
    split,

    -- ** Breaking into lines and words

    -- * View patterns
    stripPrefix,
    stripSuffix,

    -- * Searching
    filter,
    find,
    partition,

    -- * Indexing
    findIndex,

    -- * Zipping
    zip,

    -- * Conversions
    fromText,
    fromByteString,
    toText,
    toByteString,

    -- * Optics
    textWise,
    byteStringWise,
  )
where

import Control.Category ((.))
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAscii)
import Data.Coerce (coerce)
import Data.Maybe (Maybe (Just, Nothing), fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Exts (IsList (Item, fromList, fromListN, toList))
import Optics.Prism (Prism', prism')
import Text.Ascii.Char.Internal (AsciiChar (AsciiChar))
import Prelude
  ( Bool,
    Eq,
    Int,
    Monoid,
    Ord,
    Read,
    Semigroup,
    Show,
    not,
    pure,
    ($),
    (<$>),
    (>),
  )

-- Note on pragmata
--
-- This is cribbed directly from bytestring, as I figure they know what they're
-- doing way better than we do. When we add our own functionality, this probably
-- needs to be considered more carefully. - Koz

-- Type

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
      Monoid
    )
    via ByteString
  deriving stock
    ( -- | @since 1.0.0
      Show,
      -- | @since 1.0.0
      Read
    )

-- | @since 1.0.0
instance IsList AsciiText where
  type Item AsciiText = AsciiChar
  {-# INLINEABLE fromList #-}
  fromList = fromJust . fromByteString . fromList . coerce
  {-# INLINEABLE fromListN #-}
  fromListN n = fromJust . fromByteString . fromListN n . coerce
  {-# INLINEABLE toList #-}
  toList = coerce . toList . toByteString

-- Creation

-- | The empty text.
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
empty :: AsciiText
empty = coerce BS.empty

-- | A text consisting of a single ASCII character.
--
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
{-# INLINE [1] singleton #-}
singleton :: AsciiChar -> AsciiText
singleton = coerce BS.singleton

-- TODO: Quasiquoter

-- Basic interface

-- | Adds a character to the front of a text. This requires copying, which gives
-- its complexity.
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
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE snoc #-}
snoc :: AsciiText -> AsciiChar -> AsciiText
snoc = coerce BS.snoc

-- | If the argument is non-empty, gives 'Just' the first character and the
-- rest, and 'Nothing' otherwise.
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
-- /Complexity:/ \(\Theta(1)\)
--
-- @since 1.0.0
{-# INLINE unsnoc #-}
unsnoc :: AsciiText -> Maybe (AsciiText, AsciiChar)
unsnoc = coerce BS.unsnoc

-- unsnoc (AsciiText bs) = bimap AsciiText AsciiChar <$> BS.unsnoc bs

-- | The number of characters (and, since this is ASCII, bytes) in the text.
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
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE map #-}
map :: (AsciiChar -> AsciiChar) -> AsciiText -> AsciiText
map = coerce BS.map

-- | Takes a text and a list of texts, and concatenates the list after
-- interspersing the first argument between each element of the list.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE [1] intercalate #-}
intercalate :: AsciiText -> [AsciiText] -> AsciiText
intercalate = coerce BS.intercalate

-- | Takes a character, and places it between the characters of a text.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
intersperse :: AsciiChar -> AsciiText -> AsciiText
intersperse = coerce BS.intersperse

-- | Transpose the rows and columns of the argument. This uses
-- 'Data.List.transpose' internally, and thus, isn't very efficient.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
transpose :: [AsciiText] -> [AsciiText]
transpose = coerce BS.transpose

-- | Reverse the text.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
reverse :: AsciiText -> AsciiText
reverse = coerce BS.reverse

-- TODO: Replace, justifyLeft, justifyRight, center

-- Folds

-- | Left-associative fold of a text.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE foldl #-}
foldl :: (a -> AsciiChar -> a) -> a -> AsciiText -> a
foldl f x (AsciiText bs) = BS.foldl (coerce f) x bs

-- | Left-associative fold of a text, strict in the accumulator.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE foldl' #-}
foldl' :: (a -> AsciiChar -> a) -> a -> AsciiText -> a
foldl' f x (AsciiText bs) = BS.foldl' (coerce f) x bs

-- | Right-associative fold of a text.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE foldr #-}
foldr :: (AsciiChar -> a -> a) -> a -> AsciiText -> a
foldr f x (AsciiText bs) = BS.foldr (coerce f) x bs

-- | Right-associative fold of a text, strict in the accumulator.
--
-- /Complexity:/ \(\Theta(n)\)
--
-- @since 1.0.0
{-# INLINE foldr' #-}
foldr' :: (AsciiChar -> a -> a) -> a -> AsciiText -> a
foldr' f x (AsciiText bs) = BS.foldr' (coerce f) x bs

-- Special folds

concat :: [AsciiText] -> AsciiText
concat = coerce BS.concat

concatMap :: (AsciiChar -> AsciiText) -> AsciiText -> AsciiText
concatMap = coerce BS.concatMap

scanl :: (AsciiChar -> AsciiChar -> AsciiChar) -> AsciiChar -> AsciiText -> AsciiText
scanl = coerce BS.scanl

scanr :: (AsciiChar -> AsciiChar -> AsciiChar) -> AsciiChar -> AsciiText -> AsciiText
scanr = coerce BS.scanr

mapAccumL :: (a -> AsciiChar -> (a, AsciiChar)) -> a -> AsciiText -> (a, AsciiText)
mapAccumL f x (AsciiText bs) = AsciiText <$> BS.mapAccumL (coerce f) x bs

mapAccumR :: (a -> AsciiChar -> (a, AsciiChar)) -> a -> AsciiText -> (a, AsciiText)
mapAccumR f x (AsciiText bs) = AsciiText <$> BS.mapAccumL (coerce f) x bs

-- TODO: replicate

unfoldr :: (a -> Maybe (AsciiChar, a)) -> a -> AsciiText
unfoldr f = AsciiText . BS.unfoldr (coerce f)

-- TODO: unfoldrN

take :: Int -> AsciiText -> AsciiText
take = coerce BS.take

-- TODO: takeEnd

drop :: Int -> AsciiText -> AsciiText
drop = coerce BS.drop

-- TODO: dropEnd

takeWhile :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
takeWhile f (AsciiText at) = AsciiText . BS.takeWhile (coerce f) $ at

-- TODO: takeWhileEnd

dropWhile :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
dropWhile f (AsciiText at) = AsciiText . BS.dropWhile (coerce f) $ at

-- TODO: dropWhileEnd, dropAround, strip, stripStart, stripEnd

splitAt :: Int -> AsciiText -> (AsciiText, AsciiText)
splitAt = coerce BS.splitAt

-- TODO: breakOn, breakOnEnd

break :: (AsciiChar -> Bool) -> AsciiText -> (AsciiText, AsciiText)
break = coerce BS.break

span :: (AsciiChar -> Bool) -> AsciiText -> (AsciiText, AsciiText)
span = coerce BS.span

group :: AsciiText -> [AsciiText]
group = coerce BS.group

groupBy :: (AsciiChar -> AsciiChar -> Bool) -> AsciiText -> [AsciiText]
groupBy = coerce BS.groupBy

inits :: AsciiText -> [AsciiText]
inits = coerce BS.inits

tails :: AsciiText -> [AsciiText]
tails = coerce BS.tails

-- TODO: splitOn

split :: (AsciiChar -> Bool) -> AsciiText -> [AsciiText]
split = coerce BS.splitWith

-- TODO: chunksOf, lines, words, unlines, unwords

stripPrefix :: AsciiText -> AsciiText -> Maybe AsciiText
stripPrefix = coerce BS.stripPrefix

stripSuffix :: AsciiText -> AsciiText -> Maybe AsciiText
stripSuffix = coerce BS.stripSuffix

-- TODO: stripInfix, commonPrefixes

filter :: (AsciiChar -> Bool) -> AsciiText -> AsciiText
filter = coerce BS.filter

-- TODO: breakOnAll

find :: (AsciiChar -> Bool) -> AsciiText -> Maybe AsciiChar
find = coerce BS.find

partition :: (AsciiChar -> Bool) -> AsciiText -> (AsciiText, AsciiText)
partition = coerce BS.partition

-- TODO: index, safe only

findIndex :: (AsciiChar -> Bool) -> AsciiText -> Maybe Int
findIndex = coerce BS.findIndex

-- TODO: count

zip :: AsciiText -> AsciiText -> [(AsciiChar, AsciiChar)]
zip = coerce BS.zip

-- TODO: zipWith

-- Conversions

fromText :: Text -> Maybe AsciiText
fromText t = case T.find (not . isAscii) t of
  Nothing -> pure . AsciiText . encodeUtf8 $ t
  Just _ -> Nothing

fromByteString :: ByteString -> Maybe AsciiText
fromByteString bs = case BS.find (> 127) bs of
  Nothing -> pure . AsciiText $ bs
  Just _ -> Nothing

toText :: AsciiText -> Text
toText (AsciiText bs) = decodeUtf8 bs

toByteString :: AsciiText -> ByteString
toByteString = coerce

-- Prisms

textWise :: Prism' Text AsciiText
textWise = prism' toText fromText

byteStringWise :: Prism' ByteString AsciiText
byteStringWise = prism' toByteString fromByteString
