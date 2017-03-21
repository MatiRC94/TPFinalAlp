{-# LANGUAGE OverloadedStrings #-}

--import System.Console.ANSI
import Network.HTTP.Conduit (simpleHttp)
import Prelude hiding (putStrLn)
import Data.Text (Text,concat)
import Data.Text.IO (putStrLn)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor  --(Cursor, attribute, content, element, fromDocument,content, ($//), ($|), ($/) , (&//), (&/), (&|))
import Text.XML (Node (..), Element(..))
--import DatayTypes





rss = "http://www.clarin.com/rss/lo-ultimo/"
rss2= "http://www.laizquierdadiario.com/spip.php?page=backend_portada"


cursorFor :: String -> IO Cursor
cursorFor u = do
    page <- simpleHttp u
    return $ fromDocument $ parseLBS page


-- The data we're going to search for   -- esto es un axis
 -- cursor $// element "p" &/ element "a"
findNodes :: Cursor -> [Cursor]

findNodes = element "channel" &/ element "item"-- &/ element "title"   --ROSARIO3  OLE.com.ar


fun :: Cursor -> [Node]
fun c = case node c of
            NodeElement x -> (elementNodes x)
            _             -> []

--fun' :: [Cursor] -> [[Node]]
--fun' c = (map fun c)


fun2 :: [Node] -> [Cursor]
fun2 n = map fromNode n

--fun3 :: [[Node]] -> [Node]
--fun3 n = n!!1


f :: Node-> [Node]
f n =case n of
            NodeElement x -> (elementNodes x)
            _             -> []


fun4 :: [Node] -> [Node]
fun4 n = Prelude.concat (map f n)



fun5 :: IO [Node]
fun5 = cursorFor rss2 >>= \x -> return $ fun4 $ fun4 $ fun x


fun6 :: IO [Node]
fun6 = cursorFor rss2 >>= \x -> return  fun2 $ fun4 $ fun4 $ fun4 x 

--concatenar ::  Foldable t => t [a] -> [a] 




-- Extract the data from each node in turn
-- attribute :: Name -> Cursor -> [Text]
extractData :: Cursor -> Text
--content :: Cursor -> [Text] 

extractData = Data.Text.concat . content --attribute "xml:lang" --ROSARIO3  OLE.com.ar
-- Process the list of data elements
--processData :: [Data.Text.Internal.Text] -> IO ()
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
     cursor <- cursorFor rss2
     processData $ cursor $// (findNodes &| extractData)






