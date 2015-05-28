{-|
Module      : HSat.Writer
Description : High level module to write Problems to Files
Copyright   : (c) Andrew Burnett 2014
Maintainer  : andyburnett88@gmail.com
Stability   : experimental
Portability : Unknown

Provides functionality for writing 'Problem's to files
-}

module HSat.Writer (
  plainProblemToFile,
  writeFolder,
  createFileName
  ) where

import HSat.Problem
import HSat.Problem.ProblemExpr
import Data.Text.IO as T hiding (putStrLn)
import HSat.Writer.CNF
import System.Directory
import Data.Text (Text)
import HSat.Problem.ProblemType
import Control.Monad (foldM)
import HSat.Problem.Internal

{-|
Writes a 'Problem' to a 'FilePath'

The 'Bool' returned is whether this was sucessful
-}
plainProblemToFile :: Problem -> FilePath -> IO Bool
plainProblemToFile problem fp = do
  let expr = problemExpr problem
  let text = toPlainText expr
      fileName = createFileName fp expr
  exists <- doesFileExist fileName
  if exists then
    return False else
    T.writeFile fileName text >> return True

{-|
Takes an initial 'FilePath' and adds the correct suffix to it
-}
createFileName :: FilePath -> ProblemExpr -> FilePath
createFileName fp expr =
  fp ++ "." ++ fileSuffix
  where
    fileSuffix = getFileSuffix expr

getFileSuffix :: ProblemExpr -> String
getFileSuffix expr =
  case problemType expr of
    CNF -> "cnf"
    BSP -> error "Not wrtiten yet wrtier l47"

toPlainText   :: ProblemExpr -> Text
toPlainText expr =
  case problemType expr of
    CNF -> runCNFWriter . mkCNFWriter . problemToCNF $ expr
    BSP -> error "Not written yet writer l53"

{-|
Given an initial writing function, a list of problems and a folder, writes
all the functions to this folder
-}
writeFolder :: (Problem -> FilePath -> IO Bool) -> [Problem] -> FilePath ->
               IO Bool
writeFolder f problems folder = do
  folderExists <- doesDirectoryExist folder
  if folderExists then
    return False else do
      createDirectory folder
      setCurrentDirectory folder
      returnValue <- foldM f' True (zip problems [1..])
      setCurrentDirectory ".."
      return returnValue
  where
    f' :: Bool -> (Problem,Integer) -> IO Bool
    f' False _ = return False
    f' _ (p,i) = f p ("file" ++ show i)
      
