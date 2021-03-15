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
          bench "Near-miss, Text" . nf (T.count "Sebastian") $ asText,
          bench "Near-miss, AsciiText" . nf (TA.count [TA.ascii| "Sebastian" |]) $ asAsciiText,
          bench "Match 1, Text" . nf (T.count "S") $ asText,
          bench "Match 1, AsciiText" . nf (TA.count [TA.ascii| "S" |]) $ asAsciiText,
          bench "No match, Text" . nf (T.count "Simping") $ asText,
          bench "No match, AsciiText" . nf (TA.count [TA.ascii| "Simping" |]) $ asAsciiText
        ],
      bgroup
        "Replacement"
        [ bench "Replace, Text" . nf (T.replace "Sherlock" "Moriarty") $ asText,
          bench "Replace, AsciiText" . nf (TA.replace [TA.ascii| "Sherlock" |] [TA.ascii| "Moriarty" |]) $ asAsciiText,
          bench "Replace near-miss, Text" . nf (T.replace "Sebastian" "Moriarty") $ asText,
          bench "Replace near-miss, AsciiText" . nf (TA.replace [TA.ascii| "Sebastian" |] [TA.ascii| "Moriarty" |]) $ asAsciiText,
          bench "Replace 1, Text" . nf (T.replace "S" "M") $ asText,
          bench "Replace 1, AsciiText" . nf (TA.replace [TA.ascii| "S" |] [TA.ascii| "M" |]) $ asAsciiText,
          bench "Replace missing, Text" . nf (T.replace "Simping" "Dancing") $ asText,
          bench "Replace missing, AsciiText" . nf (TA.replace [TA.ascii| "Simping" |] [TA.ascii| "Dancing" |]) $ asAsciiText 
        ],
      bgroup
        "Breaking"
        [ bench "Break, Text" . nf (T.breakOn "Sherlock") $ asText,
          bench "Break, AsciiText" . nf (TA.breakOn [TA.ascii| "Sherlock" |]) $ asAsciiText,
          bench "Break near-miss, Text" . nf (T.breakOn "Sebastian") $ asText,
          bench "Break near-miss, AsciiText" . nf (TA.breakOn [TA.ascii| "Sebastian" |]) $ asAsciiText,
          bench "Break 1, Text" . nf (T.breakOn "S") $ asText,
          bench "Break 1, AsciiText" . nf (TA.breakOn [TA.ascii| "S" |]) $ asAsciiText,
          bench "Break missing, Text" . nf (T.breakOn "Simping") $ asText,
          bench "Break missing, AsciiText" . nf (TA.breakOn [TA.ascii| "Simping" |]) $ asAsciiText
        ],
      bgroup
        "Breaking (end)"
        [ bench "Break, Text" . nf (T.breakOnEnd "Sherlock") $ asText,
          bench "Break, AsciiText" . nf (TA.breakOnEnd [TA.ascii| "Sherlock" |]) $ asAsciiText,
          bench "Break near-miss, Text" . nf (T.breakOnEnd "Sebastian") $ asText,
          bench "Break near-miss, AsciiText" . nf (TA.breakOnEnd [TA.ascii| "Sebastian" |]) $ asAsciiText,
          bench "Break 1, Text" . nf (T.breakOnEnd "S") $ asText,
          bench "Break 1, AsciiText" . nf (TA.breakOnEnd [TA.ascii| "S" |]) $ asAsciiText,
          bench "Break missing, Text" . nf (T.breakOnEnd "Simping") $ asText,
          bench "Break missing, AsciiText" . nf (TA.breakOnEnd [TA.ascii| "Simping" |]) $ asAsciiText
        ],
      bgroup
        "Breaking (all)"
        [ bench "Break, Text" . nf (T.breakOnAll "Sherlock") $ asText,
          bench "Break, AsciiText" . nf (TA.breakOnAll [TA.ascii| "Sherlock" |]) $ asAsciiText,
          bench "Break near-miss, Text" . nf (T.breakOnAll "Sebastian") $ asText,
          bench "Break near-miss, AsciiText" . nf (TA.breakOnAll [TA.ascii| "Sebastian" |]) $ asAsciiText,
          bench "Break 1, Text" . nf (T.breakOnAll "S") $ asText,
          bench "Break 1, AsciiText" . nf (TA.breakOnAll [TA.ascii| "S" |]) $ asAsciiText,
          bench "Break missing, Text" . nf (T.breakOnAll "Simping") $ asText,
          bench "Break missing, AsciiText" . nf (TA.breakOnAll [TA.ascii| "Simping" |]) $ asAsciiText
        ]

    ]
