{-# LANGUAGE ExistentialQuantification, RecordWildCards  #-}

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
  ProblemParseError(..),
  -- * Functions
  fromFile,
  fromFolder,
  module HSat.Parser.Class,
  parserInstances
  ) where

import HSat.Problem
--import HSat.Problem.ProblemType (ProblemType)
--import qualified HSat.Problem.ProblemType as P
import System.Directory
import Control.Monad.Trans
import HSat.Problem.Source
import Data.List (delete)
import Control.Monad.Catch
import HSat.Parser.Class
import HSat.Problem.ProblemExpr.Class

parserInstances :: [Parser]
parserInstances = []

fromFile :: (MonadThrow m, MonadIO m) => [Parser] -> FilePath -> m Problem
fromFile [] fp = throwM $ UnrecognisedFileSuffix (getSuffix fp)
fromFile (Parser{..}:xs) fp =
  if correctSuffix fileExtension fp then do
    expression <- parser fp
    return $ MkProblem (mkFileSource fp) (ProblemExpr expression) else
    fromFile xs fp

{-|
Given a function that takes a 'FilePath' and returns a 'Problem', a folder,
applies the function to each file in the folder
-}
fromFolder :: (MonadThrow m, MonadIO m) => (FilePath -> m Problem) -> FilePath ->
              m [Problem]
fromFolder f folder = do
  exists <- liftIO $ doesDirectoryExist folder
  contents <- if exists then
                liftIO $ getDirectoryContents folder else
                return []
  let contents' = delete "." . delete ".." $ contents
  liftIO $ setCurrentDirectory folder
  xs <- mapM f contents'
  liftIO $ setCurrentDirectory ".."
  return xs

getSuffix :: FilePath -> FilePath
getSuffix fp = fp

correctSuffix :: FilePath -> FilePath -> Bool
correctSuffix _ _ = False

{-|
A sumtype describing errors
-}
data ProblemParseError =
  -- | If the file suffix is not recognised
  UnrecognisedFileSuffix String |
  ParseException String
  deriving (Eq,Show)

instance Exception ProblemParseError
