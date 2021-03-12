{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (Handle, IOMode (ReadMode), withFile)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf)
import Text.Ascii (AsciiText)
import qualified Text.Ascii as TA

main :: IO ()
main = withFile "./bench-data/big.txt" ReadMode go
  where
    go :: Handle -> IO ()
    go h = do
      asText <- TIO.hGetContents h
      case TA.fromText asText of
        Nothing -> fail "Sample data is not ASCII"
        Just asAsciiText -> runSherlockTests asText asAsciiText

-- Helpers

runSherlockTests :: Text -> AsciiText -> IO ()
runSherlockTests asText asAsciiText =
  defaultMain
    [ bgroup
        "Matching"
        [ bench "Match, Text" . nf (T.count "Sherlock") $ asText,
          bench "Match, AsciiText" . nf (TA.count [TA.ascii| "Sherlock" |]) $ asAsciiText,
          bench "Match 1, Text" . nf (T.count "a") $ asText,
          bench "Match 1, AsciiText" . nf (TA.count [TA.ascii| "a" |]) $ asAsciiText,
          bench "No match, Text" . nf (T.count "catboy") $ asText,
          bench "No match, AsciiText" . nf (TA.count [TA.ascii| "catboy" |]) $ asAsciiText
        ]
    ]
