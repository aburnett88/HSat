{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Writer.Internal (
  tests
  ) where

import TestUtils
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as T (filter)
import HSat.Writer.Internal

name :: String
name = "Internal"

tests :: TestTree
tests =
  testGroup name [
    testGroup "mkComment" [
       mkCommentTest1,
       mkCommentTest2
       ],
    testGroup "runComment" [
      runCommentTest1
      ]
    ]

mkCommentTest1 :: TestTree
mkCommentTest1 =
  testProperty "mkComment has correct text" $ property
  (\(orientation',text) ->
    let exptd = Comment orientation' (T.filter f text)
        f = \c -> c/='\n' && c/='\r'
        gotten = mkComment orientation' text
    in exptd === gotten
  )

mkCommentTest2 :: TestTree
mkCommentTest2 =
  testCase "mkComment has correct text" $ assert (exptd == gotten)
  where
    exptd = Comment Below "hello world"
    gotten = mkComment Below "hello \n\rworld"

runCommentTest1 :: TestTree
runCommentTest1 =
  testProperty "runComment returns correct values" $ property
  (\comments ->
    let belows = filter (\Comment{..} -> orientation == Below) comments
        aboves = filter (\Comment{..} -> orientation == Above) comments
        f :: Comment -> Text
        f Comment{..} = "c " <> commentText
        exptd = (map f aboves, map f belows)
        gotten = runComment comments
    in exptd === gotten
  )
             
