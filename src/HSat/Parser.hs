{-# LANGUAGE
    ExistentialQuantification,
    RecordWildCards
    #-}

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
  ProblemParseError(..)   ,
  -- * Functions
  fromFile                , -- :: (MonadThorw m, MonadIO m) => [Parser] -> FilePath -> m Problem
  fromFolder              , -- :: (MonadThrow m, MonadIo m) => (Filepath -> m Problem) -> FilePath -> m [Problem]
  module HSat.Parser.Class,
  parserInstances         ,
  ) where

import Control.Monad.Catch
import Control.Monad.Trans
import Data.List                      (delete)
import HSat.Parser.Class
import HSat.Problem
import HSat.Problem.ProblemExpr.Class
import HSat.Problem.Source
import System.Directory
import HSat.Problem.Instances.CNF.Parser (cnfParser')

parserInstances :: [Parser]
parserInstances = [
  Parser "cnf" cnfParser'
  ]
                  
{-|
Given a list of Parsers, a file, will return a problem in a monad io context
-}
fromFile                    :: (MonadThrow m, MonadIO m) => [Parser] -> FilePath -> m Problem
fromFile []              fp = throwM $ UnrecognisedFileSuffix (getSuffix fp)
fromFile (Parser{..}:xs) fp =
  if correctSuffix fileExtension fp then
    (MkProblem (mkFileSource fp) . ProblemExpr) <$> parser fp else
    fromFile xs fp

{-|
Given a function that takes a 'FilePath' and returns a 'Problem', a folder,
applies the function to each file in the folder
-}
fromFolder          :: (MonadThrow m, MonadIO m) => (FilePath -> m Problem) -> FilePath ->
                       m [Problem]
fromFolder f folder = do
  original <- liftIO getCurrentDirectory
  contents <- delete "." . delete ".." <$> liftIO (getDirectoryContents folder)
  liftIO (setCurrentDirectory folder) *> mapM f contents <* liftIO (setCurrentDirectory original)
   

getSuffix          :: FilePath -> FilePath
getSuffix []       = []
getSuffix ('.':xs) = xs
getSuffix (_:xs)   = getSuffix xs

correctSuffix              :: FilePath -> FilePath -> Bool
correctSuffix extension fp =
  let fp' = tail $ dropWhile (/='.') fp
  in fp' == extension

{-|
A sum-type describing errors
-}
data ProblemParseError =
  -- | If the file suffix is not recognised
  UnrecognisedFileSuffix String |
  ParseException String
  deriving (Eq,Show)

instance Exception ProblemParseError
