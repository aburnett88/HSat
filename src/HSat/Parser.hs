{-|
Module      : HSat.Parser
Description : Main Parsing module
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Module exports some generic Parsing functions
-}


module HSat.Parser (
  -- * Data Type
  ReadFile,
  ProblemParseError,
  runReadFile,
  -- * Functions
  fromCNFFile,
  fromFile,
  fromFolder
  ) where

import HSat.Problem
import Control.Monad.Trans.Either
import HSat.Problem.ProblemType (ProblemType)
import qualified HSat.Problem.ProblemType as P
import HSat.Problem.BSP.CNF
import System.Directory
import Data.Attoparsec.Text
import HSat.Problem.BSP.CNF.Builder
import Control.Applicative
import Data.Text.IO as T
import HSat.Parser.CNF
import Control.Monad.Trans
import HSat.Problem.ProblemExpr
import HSat.Problem.Source
import Data.List (delete)
import Control.Monad (unless)

{-|
A type wrapper around the possible Error type
-}
type ReadFile a = EitherT ProblemParseError IO a

{-|
Runs the 'ReadFile' type
-}
--Tests not needed
runReadFile :: ReadFile a -> IO (Either ProblemParseError a)
runReadFile = runEitherT

{-|
Given a 'FilePath', either returns the 'CNF' in the file or
the udnerlying error
-}
fromCNFFile :: FilePath -> ReadFile CNF
fromCNFFile fp = do
  doesFileExist' fp
  text <- lift $ T.readFile fp
  let cnf = parseOnly cnfParser text
  hoistEither $ cnfParserErrorToProblemParseError cnf

doesFileExist' :: FilePath -> ReadFile ()
doesFileExist' fp = do
  exists <- lift $ doesFileExist fp
  unless exists $ left $ FileNotFound fp

{-|
Given a 'FilePath', extracts the 'Problem' described in the file
-}
fromFile :: FilePath -> ReadFile Problem
fromFile filePath = do
  fileType <- hoistEither $ getProblemType filePath
  expr <- case fileType of
    P.CNF -> mkCNFProblem `liftA` fromCNFFile filePath
    P.BSP -> error "Not written yet parser l47"
  return $ mkProblem (mkFileSource filePath) expr

{-|
Given a function that takes a 'FilePath' and returns a 'Problem', a folder,
applies the function to each file in the folder
-}
fromFolder :: (FilePath -> ReadFile Problem) -> FilePath ->
              IO [Either ProblemParseError Problem]
fromFolder f folder = do
  exists <- doesDirectoryExist folder
  contents <- if exists then
                getDirectoryContents folder else
                return []
  let contents' = delete "." . delete ".." $ contents
  setCurrentDirectory folder
  xs <- mapM (runEitherT . f) contents'
  setCurrentDirectory ".."
  return xs

getProblemType :: FilePath -> Either ProblemParseError ProblemType
getProblemType str =
  let suffix = dropWhile (/= '.') str
  in case suffix of
    ".cnf" -> return P.CNF
    _ -> Left $ UnrecognisedFileSuffix suffix

{-|
A sumtype describing errors
-}
data ProblemParseError =
  -- | If the file suffix is not recognised
  UnrecognisedFileSuffix String |
  -- | If the file is not found
  FileNotFound FilePath |
  -- | If there is a Parser error
  CNFParserError String |
  -- | If there is a CNF Buidler error
  CNFBuildError CNFBuilderError
  deriving (Eq,Show)

cnfParserErrorToProblemParseError ::
  Either String (Either CNFBuilderError a) -> Either ProblemParseError a
cnfParserErrorToProblemParseError (Left str) =
  Left $ CNFParserError str
cnfParserErrorToProblemParseError (Right (Left err)) =
  Left $ CNFBuildError err
cnfParserErrorToProblemParseError (Right (Right a)) = Right a
