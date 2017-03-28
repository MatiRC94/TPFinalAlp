module Scraper where


import Codec.Binary.UTF8.String       (decodeString)
import Network.HTTP (getResponseBody,getRequest,simpleHTTP,defaultGETRequest_)
import Text.Feed.Import (parseFeedString)
import Text.Feed.Query (feedItems,getItemTitle,getItemLink)
import Text.Feed.Types (Item,Feed)
import Data.Maybe (Maybe)
import Network.URI (parseURI, uriToString)
import Text.Show.Unicode (ushow)

rss  = "http://www.clarin.com/rss/lo-ultimo/"
rss2 = "http://www.laizquierdadiario.com/spip.php?page=backend_portada"
rss3 = "http://contenidos.lanacion.com.ar/herramientas/rss-origen=2"
rss4 = "http://www.prensa.com/rss/section/1/"
rss5 = "https://www.pagina12.com.ar/rss/secciones/el-pais/notas"
rss6 = "http://tn.com.ar/rss.xml"      
rss7 = "http://www.telam.com.ar/rss2/ultimasnoticias.xml"
rss8 = "http://www.lapoliticaonline.com/files/rss/ultimasnoticias.xml"
rss9 = "http://www.ellitoral.com/rss/um.xml"    
rss10= "http://www.nacion.com/rss/"
rss11= "http://www.todojujuy.com/rss/todoelpais.xml"        
rss12= "http://www.neuquenalinstante.com.ar/rss/1-sociedad.xml"
rss13= "https://news.google.com.ar/news?cf=all&hl=es&pz=1&ned=es_ar&output=rss"
rss14= "http://www.eltribuno.info/rss/salta/masleidas.xml"
rss15= "http://www.perfil.com/rss/ultimomomento.xml"
rss16= "http://diariolaopinion.com.ar/rss/general.xml"
rss17= "http://www.ole.com.ar/rss/ultimas-noticias/"


-- https://github.com/bergmark/feed/tree/master/src/Text/Feed   DE ACA COMO FUNCAN LAS FUNCIONES
-- http://stackoverflow.com/questions/17038947/simple-rss-downloader-in-haskell/17044041#17044041    Parte del codigo

getTitleAndUrl :: Item -> (Maybe String, Maybe String)
getTitleAndUrl item = (getItemTitle item, getItemLink item)


--(<$>) :: Functor f => (a->b) -> f a -> f b

getTuples :: String -> IO (Maybe [(Maybe String, Maybe String)])
getTuples s = fmap (map getTitleAndUrl) <$> fmap (feedItems) <$> ( parseFeedString <$> getResponseRss s )
              --IO (Maybe[(Maybe String,Maybe String)] )
                    --IO [(Maybe String,Maybe String)]   
                                            --IO Maybe [Item]      --IO Maybe Feed     -- IO String


extractData :: Maybe [(Maybe String, Maybe String)] -> [(String,String)]
extractData (Just feedlist ) = map extract feedlist
extractData _                = error "error en el parseo de feed"

extract :: (Maybe String, Maybe String) -> (String,String)
extract (Just title,Just link) = (title,link)
extract _ = error "Error en la funcion getTuples"


getResponseRss :: String -> IO String
getResponseRss s = do 
                     s1 <- simpleHTTP (getRequest s)
                     getResponseBody s1 >>= \x -> return $ decodeString x

printTuples :: [(String, String)] -> IO ()
printTuples s = mapM_ (putStrLn.ushow) s                 

--Funcion para comprobar el scraping
probando :: String -> IO ()
probando rss = getTuples rss >>= \x -> printTuples $ extractData x


scrap :: String -> IO [(String,String)]
scrap rss = getTuples rss >>= \x -> return $ extractData x





