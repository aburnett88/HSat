{-# LANGUAGE
    ExistentialQuantification,
    RankNTypes
    #-}

{-|
Module      : HSat.Parser.Class
Description : The exports for the 'Parser' class
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

A Class for a 'Parser' to turn text files into 'Problem's
-}

module HSat.Parser.Class (
  Parser(..),
  runParser , -- :: (MonadThrow m, MonadIO m) => Parser (Either SomeException problem) ->
              --    FilePath -> m problem
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import HSat.Problem.ProblemExpr.Class
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as P

{-|
A existential type that stores Parers's for 'Problem's
-}
data Parser = forall problem. (IsProblem problem) => Parser {
  -- | The file extension of the problem
  fileExtension :: FilePath                                                    ,
  -- | The parser that turns a FilePath into a 'Problem'
  parser        :: forall m. (MonadIO m, MonadThrow m) => FilePath -> m problem
  }

{-|
Takes a Parser and a FilePath, and returns a problem
-}
runParser            :: (MonadThrow n, MonadIO n) => P.Parser (Either SomeException problem) ->
                        FilePath -> n problem
runParser parser' fp = do
  text <- liftIO $ T.readFile fp
  let result = P.parseOnly parser' text
  case result of
   Left err -> throwM $ AttoparsecError err
   Right (Left err) -> throwM err
   Right (Right res) -> return res

data AttoparsecError =
  AttoparsecError String
  deriving (Eq,Show)

instance Exception AttoparsecError
