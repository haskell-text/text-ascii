{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Text.Ascii.QQ where

import Data.Char (isAlphaNum, isAscii, isPunctuation, isSymbol, ord)
import Data.Functor (void, ($>))
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter))
import Language.Haskell.TH.Syntax
  ( Dec,
    Exp (AppE, ConE, LitE),
    Lit (IntegerL),
    Pat,
    Q,
    Type,
  )
import Text.Ascii.Char.Internal (AsciiChar (AsciiChar))
import Text.Parsec (Parsec, eof, oneOf, parse, satisfy, spaces)
import qualified Text.Parsec as Parsec

char :: QuasiQuoter
char = QuasiQuoter charQQ errPat errType errDec

charQQ :: String -> Q Exp
charQQ input = case parse go "" input of
  Left err -> fail . show $ err
  Right result ->
    pure . AppE (ConE 'AsciiChar) . LitE . IntegerL . fromIntegral $ result
  where
    go :: Parsec String () Int
    go = do
      spaces *> (void . Parsec.char $ '\'')
      c1 <- satisfy isValidLead
      case c1 of
        '\\' -> do
          c2 <- oneOf "0abfnrtv\\\'"
          Parsec.char '\'' *> spaces *> eof
            $> ord
              ( case c2 of
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
              )
        _ -> Parsec.char '\'' *> spaces *> eof $> ord c1

isValidLead :: Char -> Bool
isValidLead c = isAscii c && (isAlphaNum c || c == ' ' || isSymbol c || isPunctuation c)

errPat :: String -> Q Pat
errPat _ = fail "'char' should not be used in a pattern context."

errType :: String -> Q Type
errType _ = fail "'char' should not be used in a type context."

errDec :: String -> Q [Dec]
errDec _ = fail "'char' should not be used in a declaration context."
