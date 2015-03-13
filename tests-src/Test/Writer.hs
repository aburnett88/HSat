
module Test.Writer (
  tests
  ) where

import TestUtils
import qualified Test.Writer.CNF as CNF
import qualified Test.Writer.Internal as Internal
import Data.Either
import HSat.Writer
import HSat.Parser
import Control.Monad (liftM)
import Test.Problem ()

name :: String
name = "Writer"

tests :: TestTree
tests =
  testGroup name [
    testGroup "planProblemToFile" [
       plainProblemToFileTest1
       ],
    testGroup "writeFolder" [
      writeFolderTest1
      ],
    Internal.tests,
    CNF.tests
    ]

plainProblemToFileTest1 :: TestTree
plainProblemToFileTest1 =
  testProperty "Write random Problem to file" $ ioProperty $ do
    problem <- generate arbitrary
    success <- plainProblemToFile problem "plainProblemFileTest1"
    case success of
      True -> do
        returnProblem <- fromFile ""
        return $ case returnProblem of
          Left err -> counterexample
                      ("Unexpected Error: " ++ show err)
                      False
          Right problem' -> problem' === problem
      False -> do
        return $ counterexample "planFileWriteTest1 returned False. Please see log file" False


writeFolderTest1 :: TestTree
writeFolderTest1 =
  testProperty "Write Folder test" $ ioProperty $ do
    problems <- generate arbitrary
    success <- writeFolder plainProblemToFile problems "writeFolderTest1"
    if success then do
      returnProblems <- rights `liftM` fromFolder fromFile "writeFolderTest1"
      return $ returnProblems === problems else
      return $ counterexample "writeFolderTest1 failed. Please see log file" False
      
