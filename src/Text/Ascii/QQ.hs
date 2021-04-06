{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Text.Ascii.QQ
-- Copyright: (C) 2021 Koz Ross
-- License: Apache 2.0
-- Maintainer: Koz Ross <koz.ross@retro-freedom.nz>
-- Stability: unstable, not subject to PVP
-- Portability: GHC only
--
-- This is an internal module, and is /not/ subject to the PVP. It can change
-- in any way, at any time, and should not be depended on unless you know
-- /exactly/ what you are doing. You have been warned.
module Text.Ascii.QQ where

import Data.Char
  ( isAlphaNum,
    isAscii,
    isPunctuation,
    isSymbol,
    ord,
  )
import Data.Functor (void)
import Data.Void (Void)
import Data.Word (Word8)
import GHC.Exts (IsList (fromListN))
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter))
import Language.Haskell.TH.Syntax
  ( Dec,
    Exp (AppE, ConE, ListE, LitE, VarE),
    Lit (IntegerL),
    Pat (ConP, LitP),
    Q,
    Type,
    lift,
  )
import Text.Ascii.Internal (AsciiChar (AsciiChar))
import Text.Megaparsec
  ( Parsec,
    between,
    eof,
    lookAhead,
    manyTill,
    oneOf,
    parse,
    satisfy,
    single,
    try,
  )
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Error (errorBundlePretty)

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Text.Ascii.QQ

-- | Allows constructing ASCII characters from literals, whose correctness is
-- checked by the compiler. This can be used in both a value and a pattern
-- context; thus, the following is possible:
--
-- > case foo of
-- >    [char| '\n' |] -> doSomething
-- >    [char| '\r' |] -> doSomethingElse
-- >    _ -> don'tDoAThing
--
-- Currently, accepts literal syntax similar to the Haskell parser, with escape
-- sequences preceded by \'\\\'. In particular, this includes the single quote
-- (see the example below).
--
-- >>> [char| '\'' |]
-- '0x27'
--
-- @since 2.0.0
char :: QuasiQuoter
char = QuasiQuoter charQQ charPatQQ (errType "char") (errDec "char")

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
  Left err -> fail . errorBundlePretty $ err
  Right result -> do
    len <- lift . length $ result
    let src = fmap (fromIntegral @_ @Word8 . ord) result
    src' <- ListE <$> traverse (fmap (AppE (ConE 'AsciiChar)) . lift) src
    pure . AppE (AppE (VarE 'fromListN) len) $ src'
    {-
        let src =
              ListE
                . fmap (AppE (ConE 'AsciiChar) . LitE . IntegerL . fromIntegral . ord)
                $ result
        pure . AppE (AppE (VarE 'fromListN) len) $ src -}
  where
    open :: Parsec Void String ()
    open = space *> (void . single $ '"')
    close :: Parsec Void String ()
    close = single '"' *> space *> eof
    go :: Parsec Void String String
    go = manyTill asciiByte (lookAhead . try . single $ '"')
    asciiByte :: Parsec Void String Char
    asciiByte = do
      c <- satisfy isAscii
      case c of
        '\\' -> do
          c' <- oneOf "0abfnrtv\\\""
          pure $ case c' of
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
        _ -> pure c

charQQ :: String -> Q Exp
charQQ input = case parse (between openC closeC parseAsciiChar) "" input of
  Left err -> fail . errorBundlePretty $ err
  Right result ->
    pure . AppE (ConE 'AsciiChar) . LitE . IntegerL . fromIntegral $ result

charPatQQ :: String -> Q Pat
charPatQQ input = case parse (between openC closeC parseAsciiChar) "" input of
  Left err -> fail . errorBundlePretty $ err
  Right result ->
    pure . ConP 'AsciiChar . (: []) . LitP . IntegerL . fromIntegral $ result

openC :: Parsec Void String ()
openC = space *> (void . single $ '\'')

closeC :: Parsec Void String ()
closeC = single '\'' *> space *> eof

parseAsciiChar :: Parsec Void String Int
parseAsciiChar = do
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
