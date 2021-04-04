{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (Handle, IOMode (ReadMode), withFile)
import Test.Tasty.Bench (bcompare, bench, bgroup, defaultMain, nf)
import Test.Tasty.HUnit (assertEqual, testCase)
import Text.Ascii (AsciiText)
import qualified Text.Ascii as TA

main :: IO ()
main = withFile "./bench-data/big.txt" ReadMode go
  where
    go :: Handle -> IO ()
    go h = do
      asText <- TIO.hGetContents h
      case TA.decodeAsciiMay asText of
        Nothing -> fail "Sample data is not ASCII"
        Just asAsciiText -> runSherlockTests asText asAsciiText

-- Helpers

runSherlockTests :: Text -> AsciiText -> IO ()
runSherlockTests asText asAsciiText =
  defaultMain
    [ bgroup
        "Counting"
        [ testCase "Correctness" $ do
            assertEqual "Count" (T.count "extensive" asText) . TA.count [TA.ascii| "extensive" |] $ asAsciiText
            assertEqual "Count, dense" (T.count "condition" asText) . TA.count [TA.ascii| "condition" |] $ asAsciiText
            assertEqual "Count, near-miss" (T.count "extenzive" asText) . TA.count [TA.ascii| "extenzive" |] $ asAsciiText
            assertEqual "Count, long" (T.count "incomprehensible" asText) . TA.count [TA.ascii| "incomprehensible" |] $ asAsciiText
            assertEqual "Count, run" (T.count "----------" asText) . TA.count [TA.ascii| "----------" |] $ asAsciiText
            assertEqual "Count 5" (T.count "which" asText) . TA.count [TA.ascii| "which" |] $ asAsciiText
            assertEqual "Count 4" (T.count "then" asText) . TA.count [TA.ascii| "then" |] $ asAsciiText
            assertEqual "Count 3" (T.count "two" asText) . TA.count [TA.ascii| "two" |] $ asAsciiText
            assertEqual "Count 2" (T.count "of" asText) . TA.count [TA.ascii| "of" |] $ asAsciiText
            assertEqual "Count 1" (T.count "Z" asText) . TA.count [TA.ascii| "Z" |] $ asAsciiText
            assertEqual "Count 1, dense" (T.count "e" asText) . TA.count [TA.ascii| "e" |] $ asAsciiText
            assertEqual "Count, no match" (T.count "Azathothi" asText) . TA.count [TA.ascii| "Azathothi" |] $ asAsciiText,
          bench "Count, AsciiText" . nf (TA.count [TA.ascii| "extensive" |]) $ asAsciiText,
          bcompare "$NF == \"Count, AsciiText\"" . bench "Count, Text" . nf (T.count "extensive") $ asText,
          bench "Count, dense, AsciiText" . nf (TA.count [TA.ascii| "condition" |]) $ asAsciiText,
          bcompare "$NF == \"Count, dense, AsciiText\"" . bench "Count, dense, Text" . nf (T.count "condition") $ asText,
          bench "Count, near-miss, AsciiText" . nf (TA.count [TA.ascii| "extenzive" |]) $ asAsciiText,
          bcompare "$NF == \"Count, near-miss, AsciiText\"" . bench "Count, near-miss, Text" . nf (T.count "extenzive") $ asText,
          bench "Count, long, AsciiText" . nf (TA.count [TA.ascii| "incomprehensible" |]) $ asAsciiText,
          bcompare "$NF == \"Count, long, AsciiText\"" . bench "Count, long, Text" . nf (T.count "incomprehensible") $ asText,
          bench "Count, run, AsciiText" . nf (TA.count [TA.ascii| "---------" |]) $ asAsciiText,
          bcompare "$NF == \"Count, run, AsciiText\"" . bench "Count, run, Text" . nf (T.count "---------") $ asText,
          bench "Count 5, AsciiText" . nf (TA.count [TA.ascii| "which" |]) $ asAsciiText,
          bcompare "$NF == \"Count 5, AsciiText\"" . bench "Count 5, Text" . nf (T.count "which") $ asText,
          bench "Count 4, AsciiText" . nf (TA.count [TA.ascii| "then" |]) $ asAsciiText,
          bcompare "$NF == \"Count 4, AsciiText\"" . bench "Count 4, Text" . nf (T.count "then") $ asText,
          bench "Count 3, AsciiText" . nf (TA.count [TA.ascii| "two" |]) $ asAsciiText,
          bcompare "$NF == \"Count 3, AsciiText\"" . bench "Count 3, Text" . nf (T.count "two") $ asText,
          bench "Count 2, AsciiText" . nf (TA.count [TA.ascii| "of" |]) $ asAsciiText,
          bcompare "$NF == \"Count 2, AsciiText\"" . bench "Count 2, Text" . nf (T.count "of") $ asText,
          bench "Count 1, AsciiText" . nf (TA.count [TA.ascii| "Z" |]) $ asAsciiText,
          bcompare "$NF == \"Count 1, AsciiText\"" . bench "Count 1, Text" . nf (T.count "Z") $ asText,
          bench "Count 1, dense, AsciiText" . nf (TA.count [TA.ascii| "e" |]) $ asAsciiText,
          bcompare "$NF == \"Count 1, dense, AsciiText\"" . bench "Count 1, dense, Text" . nf (T.count "e") $ asText,
          bench "Count, no match, AsciiText" . nf (TA.count [TA.ascii| "Azathoth" |]) $ asAsciiText,
          bcompare "$NF == \"Count, no match, AsciiText\"" . bench "Count, no match, Text" . nf (T.count "Azathoth") $ asText
        ]
    ]

{-
  bgroup
    "Replacement"
    [ bench "Replace, Text" . nf (T.replace "Sherlock" "Moriarty") $ asText,
      bcompare "$NF == \"Replace, Text\"" . bench "Replace, AsciiText" . nf (TA.replace [TA.ascii| "Sherlock" |] [TA.ascii| "Moriarty" |]) $ asAsciiText,
      bench "Replace near-miss, Text" . nf (T.replace "Sebastian" "Moriarty") $ asText,
      bcompare "$NF == \"Replace near-miss, Text\"" . bench "Replace near-miss, AsciiText" . nf (TA.replace [TA.ascii| "Sebastian" |] [TA.ascii| "Moriarty" |]) $ asAsciiText,
      bench "Replace 1, Text" . nf (T.replace "S" "M") $ asText,
      bcompare "$NF == \"Replace 1, Text\"" . bench "Replace 1, AsciiText" . nf (TA.replace [TA.ascii| "S" |] [TA.ascii| "M" |]) $ asAsciiText,
      bench "Replace missing, Text" . nf (T.replace "Simping" "Dancing") $ asText,
      bcompare "$NF == \"Replace missing, Text\"" . bench "Replace missing, AsciiText" . nf (TA.replace [TA.ascii| "Simping" |] [TA.ascii| "Dancing" |]) $ asAsciiText
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
] -}
