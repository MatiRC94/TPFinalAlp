{-# LANGUAGE OverloadedStrings #-}

--import System.Console.ANSI
import Network.HTTP.Conduit (simpleHttp)
import Prelude hiding (concat, putStrLn)
import Data.Text (concat)
import Data.Text.IO (putStrLn)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attribute, content, element, fromDocument, ($//), ($|), ($/) , (&//), (&/), (&|))

-- The URL we're going to search
url  = "http://www.ole.com.ar"
url1 = "http://www.laizquierdadiario.com"
url2 = "http://www.rosario3.com"
url3 = "http://www.clarin.com"
url4 = "http://www.lacapital.com.ar"
url5 = "http://www.perfil.com"

-- data Urls = [String]
-- TODO Scrapear concurrentemente

cursorFor :: String -> IO Cursor
cursorFor u = do
    page <- simpleHttp u
    return $ fromDocument $ parseLBS page


-- Axis in Text.XML.Cursor are type synonym of Cursor -> [Cursor]:

-- (&/) :: Axis node -> (Cursor node -> [a]) -> Cursor node -> [a]
-- Combine two axes so that the second works on the children of the results of the first.


-- The data we're going to search for   -- esto es un axis
 -- cursor $// element "p" &/ element "a"
--findNodes :: Cursor -> [Cursor]
-- *****************
findNodes = element "figure" &/ element "a"    --ROSARIO3  OLE.com.ar
--findNodes = element "article" &/ element "a"
--findNodes = element "h2" &/ element "a"
-- ********************

-- Extract the data from each node in turn
-- attribute :: Name -> Cursor -> [Text]
-- extractData :: Cursor -> Text

extractData2 = concat . content 

extractData = concat . attribute "alt" --ROSARIO3  OLE.com.ar
-- Process the list of data elements
processData = mapM_ putStrLn

{-
($//) :: Cursor node -> (Cursor node -> [Text]) -> [Text]
Apply an axis to the descendants of a 'Cursor node'.

(&|) :: (Cursor node -> [Cursor]) -> (Cursor -> Text) -> Cursor node -> [Text]
Apply a function to the result of an axis.

Cursor Node -> [Text]
:t findNodes &| extractData
-}
parseAF :: IO ()
parseAF = do
     cursor <- cursorFor url4
     processData $ cursor $// (findNodes &| extractData)



parseAF2 = do
      cursor <- cursorFor url4
      processData $ cursor $// (findNodes &| extractData2)


--- LANACION Y LA IZQUIERDADIARIO Lacapital  ambas h2 y problema del   > y sin title

{-
findNodes = element "header" &/ element "a" mas leidas la capital
extractData = concat . attribute "alt" mas leidas lacapital

-}


{-
findNodes = element "h1" &/ element "a" --ROSARIO3  OLE.com.ar

extractData = concat . attribute "title" --ROSARIO3  OLE.com.ar
-}


{-

findNodes = element "a" &/ element "img"        --titulos la Izq  (coinciden foto con titulo
extractData = concat . attribute "alt"          --titulos la Izq 


-}

{-

findNodes = element "figure" &/ element "a"

extractData = concat . attribute "alt"

-}
