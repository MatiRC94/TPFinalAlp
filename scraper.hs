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
findNodes = element "title" -- &/ element "item" -- &/ element "title"   --ROSARIO3  OLE.com.ar


fun :: [Cursor] -> [Node]
fun c = Prelude.concat (map (\x -> case node x of
                                        NodeElement x -> (elementNodes x)
                                        _             -> [] ) c)

fun2 :: [Node] -> [Cursor]
fun2 n = map fromNode n


f :: Node-> [Node]
f n =case n of
            NodeElement x -> (elementNodes x)
            _             -> []

fun4 :: [Node] -> [Node]
fun4 n = Prelude.concat (map f n)


fun6 :: [Cursor] -> [Cursor]
fun6 c = fun2 $ fun c

--fun6 :: IO [[Node]]
--fun6 = cursorFor rss2 >>= \x -> return (Prelude.concat $ (fun5(fun4 (fun x))) )

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

findNodes2 :: Cursor -> [Cursor]
findNodes2 x = fun6 $ findNodes x

parseAF2 :: IO ()
parseAF2 = do
     cursor <- cursorFor rss2
     processData $ cursor $// ( findNodes2 &| extractData)


