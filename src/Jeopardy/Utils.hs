-- Copyright (C) 2013-2015 Moritz Schulte <mtesseract@silverratio.net>

module Jeopardy.Utils where

import Data.Char                    -- maybeRead
import Data.Maybe                   -- And of course the Maybe type.
import qualified Data.Text as T -- strip

maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst
            . listToMaybe
            . filter (null . snd)
            . reads

isDigit' :: String -> Bool
isDigit' [] = False
isDigit' (a:_) = isDigit a

-- Construct cartesian Product of two sets.
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

dataExportToFile :: (Show a) => a -> FilePath -> IO ()
dataExportToFile dat filename = do
  writeFile filename (show dat)

dataImportFromFile :: (Read a) => FilePath -> IO (Maybe a)
dataImportFromFile filename = do
  content <- readFile filename
  return $ maybeRead content

trimWhitespaces :: String -> String
trimWhitespaces = T.unpack . T.strip . T.pack
