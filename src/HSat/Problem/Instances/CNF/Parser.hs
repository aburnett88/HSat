{-|
Module      : HSat.Problem.Instances.CNF.Parser
Description : The Parser for the CNF file format
Copyright   : (c) Andrew Burnett 2014-2015
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Module containing the 'Parser' for the CNF file format
-}

module HSat.Problem.Instances.CNF.Parser (
  cnfParser , -- :: Parser (Either CNFBuilderError CNF)
  cnfParser', -- :: (MonadIO, MonadThrow m) => FilePath -> m CNF
  ) where

import Control.Monad.Catch
import HSat.Problem.Instances.CNF
import HSat.Problem.Instances.CNF.Builder
import HSat.Problem.Instances.CNF.Parser.Internal
import Control.Monad.IO.Class
import Data.Attoparsec.Text
import qualified Data.Text.IO as T

{-|
Parser that parses a CNF file in 'Data.Text' form, and produces either a 'CNFBuilderError' or a 'CNF'
-}
cnfParser :: (MonadThrow m) => Parser (m CNF)
cnfParser =
  (finalise =<<) <$> (
    parseComment >> parseComments >> parseProblemLine >>= parseClauses) <* endOfInput

cnfParser' :: (MonadIO m, MonadThrow m) => FilePath -> m CNF
cnfParser' fp = do
  text <- liftIO $ T.readFile fp
  let result = parseOnly cnfParser text
  case result of
   Left err -> throwM $ AttoparsecError err
   Right (Left err) -> throwM err
   Right (Right res) -> return res

data AttoparsecError =
  AttoparsecError String
  deriving (Eq,Show)

instance Exception AttoparsecError
