module HSat.Parser (
  fromCNFFile,
  fromFile,
  fromFolder,
  runReadFile
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

type ReadFile a = EitherT ProblemParseError IO a

runReadFile :: ReadFile a -> IO (Either ProblemParseError a)
runReadFile = runEitherT

fromCNFFile :: FilePath -> ReadFile CNF
fromCNFFile fp = do
  doesFileExist' fp
  text <- lift $ T.readFile fp
  let cnf = parseOnly cnfParser text
  hoistEither $ cnfParserErrorToProblemParseError cnf

doesFileExist' :: FilePath -> ReadFile ()
doesFileExist' fp = do
  exists <- lift $ doesFileExist fp
  case exists of
    True -> return ()
    False -> left $ (FileNotFound fp)

fromFile :: FilePath -> ReadFile Problem
fromFile filePath = do
  fileType <- hoistEither $ getProblemType filePath
  expr <- case fileType of
    P.CNF -> mkCNFProblem `liftA` fromCNFFile filePath
  return $ mkProblem (mkFileSource filePath) expr

fromFolder :: (FilePath -> ReadFile Problem) -> FilePath -> IO [Either ProblemParseError Problem]
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

data ProblemParseError =
  UnrecognisedFileSuffix String |
  FileNotFound FilePath |
  CNFParserError String |
  CNFBuildError CNFBuilderError
  deriving (Eq,Show)

cnfParserErrorToProblemParseError ::
  Either String (Either CNFBuilderError a) -> Either ProblemParseError a
cnfParserErrorToProblemParseError (Left str) =
  Left $ CNFParserError str
cnfParserErrorToProblemParseError (Right (Left err)) =
  Left $ CNFBuildError err
cnfParserErrorToProblemParseError (Right (Right a)) = Right a
