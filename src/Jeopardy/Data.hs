-- Copyright (C) 2013-2015 Moritz Schulte <mtesseract@silverratio.net>

module Jeopardy.Data where

import qualified Data.ByteString as B
import qualified Data.Map as M

import Jeopardy.Utils
import Data.Char -- toUpper, ord

type Category   = String
type Price      = Integer
data JItem      = JItem { jitemAnswer   :: String,
                          jitemQuestion :: String,
                          jitemDouble   :: Bool
                        } deriving (Read, Show)
type JDataTable = M.Map (Category, Price) JItem
data JData      = JData { jdataName       :: String,
                          jdataCategories :: [Category],
                          jdataPreamble   :: String,
                          jdataTable      :: JDataTable
                        } deriving (Show, Read)

-- Compiled Version. We basically compile JItems with LaTeX to JItemsCompiled.

data JItemCompiled = JItemCompiled { jitemAnswer_   :: B.ByteString,
                                     jitemQuestion_ :: B.ByteString,
                                     jitemDouble_   :: Bool
                                   } deriving (Read, Show)
type JDataTableCompiled = M.Map (Category, Price) JItemCompiled
data JDataCompiled =
  JDataCompiled { jdataName_       :: String,
                  jdataCategories_ :: [Category],
                  jdataTable_      :: JDataTableCompiled
                } deriving (Show, Read)
data JItemMeta =
  JItemMeta { jitemMetaDouble :: Bool } deriving (Show, Read)

_Categories :: [String]
_Categories = map (: []) ['a' .. 'e']

cardIndex' :: [(String, Integer)]
cardIndex' = cartProd _Categories [1..5]

cardIndex :: [String]
cardIndex =
  map (\ (category, price) -> (category ++ show price)) cardIndex'
             
jidToPrice :: Price -> Integer
jidToPrice jid = toInteger (jid * 100)

categoryIdx :: Category -> Int
categoryIdx category = (ord (head category)) - (ord 'a')

cardIndex'' :: String -> (Category, Price)
cardIndex'' s =
  let cat = (s !! 0) : []
      jid = ord (s !! 1)
  in (cat, jidToPrice (fromIntegral jid))

jeopardyGtkBuilder :: String
jeopardyGtkBuilder  = "jeopardy.glade"
