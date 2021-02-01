{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Text.Ascii.QQ where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
  ( isAlphaNum,
    isAscii,
    isPunctuation,
    isSymbol,
    ord,
  )
import Data.Functor (void)
import GHC.Exts (IsList (fromList))
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter))
import Language.Haskell.TH.Syntax
  ( Dec,
    Exp (AppE, ConE, ListE, LitE, VarE),
    Lit (IntegerL),
    Pat,
    Q,
    Type,
  )
import Text.Ascii.Internal (AsciiChar (AsciiChar), AsciiText (AsciiText))
import Text.Parsec
  ( Parsec,
    between,
    eof,
    lookAhead,
    manyTill,
    oneOf,
    parse,
    satisfy,
    spaces,
    try,
  )
import qualified Text.Parsec as Parsec

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Text.Ascii.QQ

-- | Allows constructing ASCII characters from literals, whose correctness is
-- checked by the compiler.
--
-- Currently, accepts literal syntax similar to the Haskell parser, with escape
-- sequences preceded by \'\\\'. In particular, this includes the single quote
-- (see the example below).
--
-- >>> [char| '\'' |]
-- '0x27'
--
-- @since 1.0.0
char :: QuasiQuoter
char = QuasiQuoter charQQ (errPat "char") (errType "char") (errDec "char")

-- | Allows constructing ASCII strings from literals, whose correctness is
-- checked by the compiler.
--
-- Currently accepts literal syntax similar to the Haskell parser, with escape
-- sequences preceded by \'\\\'. In particular, this includes the double quote
-- (see the example below).
--
-- >>> [ascii| "\"Nyan!\", said the catboy." |]
-- "\"Nyan!\", said the catboy."
--
-- @since 1.0.0
ascii :: QuasiQuoter
ascii = QuasiQuoter asciiQQ (errPat "ascii") (errType "ascii") (errDec "ascii")

-- Helpers

asciiQQ :: String -> Q Exp
asciiQQ input = case parse (between open close go) "" input of
  Left err -> fail . show $ err
  Right result ->
    pure
      . AppE (ConE 'AsciiText)
      . AppE (VarE 'fromList)
      . ListE
      . fmap (LitE . IntegerL . fromIntegral)
      . BS.unpack
      $ result
  where
    open :: Parsec String () ()
    open = spaces *> (void . Parsec.char $ '"')
    close :: Parsec String () ()
    close = Parsec.char '"' *> spaces *> eof
    go :: Parsec String () ByteString
    go = BS.pack <$> manyTill asciiByte (lookAhead . try . Parsec.char $ '"')
    asciiByte = do
      c <- satisfy isAscii
      case c of
        '\\' -> do
          c' <- oneOf "0abfnrtv\\\""
          pure . fromIntegral . ord $ case c' of
            '0' -> '\0'
            'a' -> '\a'
            'b' -> '\b'
            'f' -> '\f'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            'v' -> '\v'
            '\\' -> '\\'
            _ -> '"'
        _ -> pure . fromIntegral . ord $ c

charQQ :: String -> Q Exp
charQQ input = case parse (between open close go) "" input of
  Left err -> fail . show $ err
  Right result ->
    pure . AppE (ConE 'AsciiChar) . LitE . IntegerL . fromIntegral $ result
  where
    open :: Parsec String () ()
    open = spaces *> (void . Parsec.char $ '\'')
    close :: Parsec String () ()
    close = Parsec.char '\'' *> spaces *> eof
    go :: Parsec String () Int
    go = do
      c1 <- satisfy isValidLead
      case c1 of
        '\\' -> do
          c2 <- oneOf "0abfnrtv\\\'"
          pure . ord $ case c2 of
            '0' -> '\0'
            'a' -> '\a'
            'b' -> '\b'
            'f' -> '\f'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            'v' -> '\v'
            '\\' -> '\\'
            _ -> '\''
        _ -> pure . ord $ c1

isValidLead :: Char -> Bool
isValidLead c = isAscii c && (isAlphaNum c || c == ' ' || isSymbol c || isPunctuation c)

errPat :: String -> String -> Q Pat
errPat name _ = fail $ "'" <> name <> "' should not be used in a pattern context."

errType :: String -> String -> Q Type
errType name _ = fail $ "'" <> name <> "' should not be used in a type context."

errDec :: String -> String -> Q [Dec]
errDec name _ = fail $ "'" <> name <> "' should not be used in a declaration context."
