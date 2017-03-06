module Scrapping where

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, content, element, fromDocument, child,
                        (&/), ($//), (&|), (>=>))

-- The data we're going to search for
findNodes :: Cursor -> [Cursor]
findNodes = element "book" &// element "price" >=> child

-- Extract the data from each node in turn
extractData = T.concat . content

-- Process the list of data elements
processData =  mapM_ T.putStrLn

-- test
main = do
     let cursor = fromDocument $ parseLBS "<xml><bookstore><book><price>29.99</price></book><book><price>31.99</price></book></bookstore></xml>"
     processData $ cursor $// findNodes &| extractData
