-- Copyright (C) 2013-2015 Moritz Schulte <mtesseract@silverratio.net>

module Main (main) where

import System.IO.Temp
import System.IO
import qualified Data.ByteString as B
import qualified Data.Map as M
import System.Directory
import Data.Maybe
import System.Environment           -- Commandline Arguments.
import qualified Codec.Archive.Tar as Tar

import Jeopardy.Data
import Jeopardy.Utils
import Jeopardy.Compiler

compileJItem' :: String -> JItem -> FilePath -> IO (Maybe JItemCompiled)
compileJItem' preamble jitem directory = do
  oldCwd <- getCurrentDirectory
  setCurrentDirectory directory
  let question = jitemQuestion jitem
      answer   = jitemAnswer   jitem
      isDouble = jitemDouble   jitem
  maybeAnswer   <- compileOne "jitem-answer" (textWrap preamble answer oldCwd)
  maybeQuestion <- if isJust maybeAnswer
                   then compileOne "jitem-question"
                          (textWrap preamble question oldCwd)
                   else return Nothing
  setCurrentDirectory oldCwd
  if isJust maybeAnswer && isJust maybeQuestion
    then do let question' = fromJust maybeQuestion
                answer'   = fromJust maybeAnswer
                jitem'    = JItemCompiled { jitemAnswer_   = answer',
                                            jitemQuestion_ = question',
                                            jitemDouble_   = isDouble }
            return $ Just jitem'
    else return Nothing

compileJItem :: String -> JItem -> IO (Maybe JItemCompiled)
compileJItem preamble jitem =
  withTempDirectory "/tmp" "jitem.tmp" (compileJItem' preamble jitem)

compileProcessJItem :: String -> (Category, Price) -> JItem ->
                       IO (Maybe JDataTableCompiled) ->
                       IO (Maybe JDataTableCompiled)
compileProcessJItem preamble key val tableCompiled' = do
  maybeTableCompiled <- tableCompiled'
  if isNothing maybeTableCompiled
    then return Nothing
    else do let tableCompiled = fromJust maybeTableCompiled
            putStr $ "Compiling " ++ show key ++ "... "
            hFlush stdout
            jitem' <- compileJItem preamble val
            if isJust jitem'
              then do putStrLn "done"
                      let jitem = fromJust jitem'
                          tableNew = M.insert key jitem tableCompiled
                      return $ Just tableNew
              else do putStrLn "Failed to compile!"
                      return Nothing

compileJDataTable :: String -> JDataTable -> IO (Maybe JDataTableCompiled)
compileJDataTable preamble =
  M.foldrWithKey (compileProcessJItem preamble) (return (Just M.empty))

compileJData :: JData -> IO (Maybe JDataCompiled)
compileJData jdata = do
  let name        = jdataName       jdata
      preamble    = jdataPreamble   jdata
      categories  = jdataCategories jdata
      jdatatable  = jdataTable      jdata
  jdatatable' <- compileJDataTable preamble jdatatable
  if isJust jdatatable'
    then return $ Just JDataCompiled { jdataName_       = name
                                     , jdataCategories_ = categories
                                     , jdataTable_      = fromJust jdatatable' }
    else return Nothing

createJItemFilenameAnswer :: FilePath -> (Category, Price) -> FilePath
createJItemFilenameAnswer dirName (category, price) =
  dirName ++ "/" ++ "jitem-" ++ category ++ show price ++ "-answer.png"

createJItemFilenameQuestion :: FilePath -> (Category, Price) -> FilePath
createJItemFilenameQuestion dirName (category, price) =
  dirName ++ "/" ++ "jitem-" ++ category ++ show price ++ "-question.png"

createJItemFilenameMeta :: FilePath -> (Category, Price) -> FilePath
createJItemFilenameMeta dirName (category, price) =
  dirName ++ "/" ++ "jitem-" ++ category ++ show price

-- Export a JItem into the specified directory.
exportJItem :: FilePath -> (Category, Price) -> JItemCompiled -> IO ()
exportJItem dirName (category, price) jitem = do
  let nameAnswer = createJItemFilenameAnswer dirName
                                             (category, price)
      nameQuestion = createJItemFilenameQuestion dirName
                                                 (category, price)
  B.writeFile nameAnswer (jitemAnswer_ jitem)
  B.writeFile nameQuestion (jitemQuestion_ jitem)
  -- Write JItem meta information.
  let nameMeta = createJItemFilenameMeta dirName (category, price)
      metaData = JItemMeta { jitemMetaDouble = jitemDouble_ jitem }
  writeFile nameMeta $ show metaData

-- Export a JDataCompiled data structure into the specified file.
jdataExport :: JDataCompiled -> FilePath -> IO ()
jdataExport jdata filename = do
  let jdataStripped = jdata { jdataTable_ = M.empty }
      jdatatable =  jdataTable_ jdata
  withSystemTempDirectory "tmp-jeopardy.jdc"
    (\ tmpDir -> do
        writeFile (tmpDir ++ "/meta") $ show jdataStripped
        mapM_ (\ (category, price) -> do
                  let maybeJItem = M.lookup (category, price) jdatatable
                      jitem = fromJust maybeJItem
                  exportJItem tmpDir (category, price) jitem)
          (M.keys jdatatable)
        Tar.create filename tmpDir ["."])
    
main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileIn, fileOut] -> do src <- readFile fileIn
                            let jdata = maybeRead src
                            if isNothing jdata -- == Nothing
                              then putStrLn "failed to parse input"
                              else do jdata' <- compileJData (fromJust jdata)
                                      if isJust jdata'
                                        then jdataExport (fromJust jdata') fileOut
                                        else putStrLn "failed to compile input"
    _ -> putStrLn "Usage: compiler <input file> <output file>"
