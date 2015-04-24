{-|
Module      : TestPrint
Description : The general Printing Test Functions
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

This module provides the general functions required for writing Printing Tests
-}


module TestPrint (
  printTest,  -- :: (Printer a) => String -> a -> TestTree
  TestTree,   -- :: TestTree
  testGroup,  -- :: String -> [TestTree] -> TestTree
  printList,  -- :: (Printer a) => String -> [a] -> TestTree
  defaultMain -- :: [TestTree] -> IO ()
  ) where

import Test.Tasty.HUnit
import Test.Tasty
import HSat.Printer

{-|
Takes a Printable element and prints it in all its forms
-}
printTest       :: (Printer a) => String -> a -> TestTree
printTest str x =
  testGroup str [
    func "Compact"    (compact x),
    func "No Unicode" (noUnicode x),
    func "Unicode"    (unicode x)
    ]
  where
    func         :: String -> Doc -> TestTree
    func str' doc =
      testCase str' $ do
        putStrLn ""
        putDoc doc
        putStrLn ""
        assertBool "" True

{-|
Takes a list of Printable elements and prints them all in all their forms
-}
printList :: (Printer a) => String -> [a] -> TestTree
printList str xs =
  testGroup str $ map (printTest "") xs
