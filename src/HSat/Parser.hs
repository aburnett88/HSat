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
import Control.Applicative
import Data.Text.IO as T
import HSat.Parser.CNF
import Control.Monad.Trans
import HSat.Problem.ProblemExpr
import HSat.Problem.Source
import Data.List (delete)
import Control.Monad.Catch

{-|
A type wrapper around the possible Error type
-}
type ReadFile a = EitherT ProblemParseError IO a

{-|
Runs the 'ReadFile' type
-}
--Tests not needed
--runReadFile :: (MonadIO m, MonadThrow m) => m a -> m a
--runReadFile = runEitherT

runReadFile :: ReadFile a -> IO (Either ProblemParseError a)
runReadFile = runEitherT

{-|
Given a 'FilePath', either returns the 'CNF' in the file or
the udnerlying error
-}
fromCNFFile :: (MonadThrow m, MonadIO m) => FilePath -> m CNF
fromCNFFile fp = do
  text <- liftIO $ T.readFile fp
  case parseOnly cnfParser text of
   Left parserException -> throwM $ ParseException parserException
   Right (Left builderException) -> throwM builderException
   Right (Right cnf) -> return cnf

{-
-}
{-|
Given a 'FilePath', extracts the 'Problem' described in the file
-}
fromFile :: (MonadThrow m, MonadIO m) => FilePath -> m Problem
fromFile filePath = do
  fileType <- getProblemType filePath
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

getProblemType :: (MonadThrow m) => FilePath -> m ProblemType
getProblemType str =
  let suffix = dropWhile (/= '.') str
  in case suffix of
    ".cnf" -> return P.CNF
    _ -> throwM $ UnrecognisedFileSuffix suffix


{-|
A sumtype describing errors
-}
data ProblemParseError =
  -- | If the file suffix is not recognised
  UnrecognisedFileSuffix String |
  ParseException String
  deriving (Eq,Show)

instance Exception ProblemParseError
