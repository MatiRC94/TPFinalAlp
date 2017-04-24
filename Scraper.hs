module Scraper where


import Codec.Binary.UTF8.String       (decodeString)
import Network.HTTP (getResponseBody,getRequest,simpleHTTP,defaultGETRequest_)
import Text.Feed.Import (parseFeedString)
import Text.Feed.Query (feedItems,getItemTitle,getItemLink,getFeedTitle)
import Text.Feed.Types (Item,Feed)
import Data.Maybe (Maybe)
import Network.URI (parseURI, uriToString)
import Text.Show.Unicode (ushow)


-- https://github.com/bergmark/feed/tree/master/src/Text/Feed   Info sobre las funciones
-- http://stackoverflow.com/questions/17038947/simple-rss-downloader-in-haskell/17044041#17044041   parte del codigo

getTitleAndUrl :: Item -> (Maybe String, Maybe String)
getTitleAndUrl item = (getItemTitle item, getItemLink item)


--(<$>) :: Functor f => (a->b) -> f a -> f b
--Obtener las tuplas con la informacion del titulo y la Url
getTuples :: String -> IO (Maybe [(Maybe String, Maybe String)])
getTuples s = fmap (map getTitleAndUrl) <$> fmap (feedItems) <$> ( parseFeedString <$> getResponseRss s )
              --IO (Maybe[(Maybe String,Maybe String)] )
                    --IO [(Maybe String,Maybe String)]   
                                            --IO Maybe [Item]      --IO Maybe Feed     -- IO String

--Extraer los datos tomados de getTuples
extractData :: Maybe [(Maybe String, Maybe String)] -> [(String,String)]
extractData (Just feedlist ) = map extract feedlist
extractData _                = error "error en el parseo de feed"

--Para ver errores y sacar el Just
extract :: (Maybe String, Maybe String) -> (String,String)
extract (Just title,Just link) = (title,link)
extract _ = error "Error en el Screapeo, puede que la url tenga errores"

--A partir de una Url, obtengo la informacion decodificada
getResponseRss :: String -> IO String
getResponseRss s = do 
                     s1 <- simpleHTTP (getRequest s)
                     getResponseBody s1 >>= \x -> return $ decodeString x

--Imprime las tuplas
printTuples :: [(String, String)] -> IO ()
printTuples s = mapM_ (putStrLn.ushow) s                 

--Funcion para probar el scraping
probando :: String -> IO ()
probando rss = getTuples rss >>= \x -> printTuples $ extractData x

--Funcion de scraping
scrap :: String -> IO [(String,String)]
scrap rss = getTuples rss >>= \x -> return $ extractData x

