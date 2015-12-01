-- Copyright (C) 2013-2015 Moritz Schulte <mtesseract@silverratio.net>

module Main (main) where

import qualified Data.Map as M
import Data.Maybe
import System.Environment           -- Commandline Arguments.

import Jeopardy.Data
import Jeopardy.Utils

dumpJDataItem :: JDataTable -> Category -> Integer -> IO ()
dumpJDataItem table category n = do
  let pair = (category, n)
  let jitem = M.lookup pair table
  -- abort if jitem is nothing
  let jitem' = fromJust jitem
  putStr $ " " ++ show n ++ ": [A] " ++ jitemAnswer jitem' ++ "\n" ++
           "    [Q] " ++ jitemQuestion jitem' ++ "\n"


dumpJDataCategory :: JData -> Category -> IO ()
dumpJDataCategory jdata category = do
  let table = jdataTable jdata
  let catIdx = categoryIdx category
  putStr $ "\nCategory: " ++ jdataCategories jdata !! catIdx ++ "\n--------------------\n"
  mapM_ (dumpJDataItem table category) [1..5]

dumpJDataTable :: JData -> IO ()
dumpJDataTable jdata = 
  mapM_ (\ category -> dumpJDataCategory jdata [category]) ['a'..'e']

dumpJData :: JData -> IO ()
dumpJData jdata = do
  putStr $ "Datasheet: " ++ jdataName jdata ++ "\n" ++ "====================" ++ "\n"
  dumpJDataTable jdata
    
main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileIn] -> do src <- readFile fileIn
                   let jdata = maybeRead src
                   maybe (putStrLn "failed to parse input") dumpJData jdata
    _ -> putStrLn "Usage: dump <data file>"
