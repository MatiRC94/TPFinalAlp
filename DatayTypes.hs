
module DatayTypes where

import Network.HTTP
import Network.Stream
import Control.Exception
data Priority = Alta [Url] | Media [Url] | Baja [Url] deriving Show

type Url = String

addUrls :: Url -> Priority -> Priority
addUrls s (Alta p)  = Alta (s : p)
addUrls s (Media p)  = Media (s : p)
addUrls s (Baja p)  = Baja (s : p)



--removeUrls :: Url -> Priority
--removeUrls s = 


listUrls :: Priority -> IO ()
listUrls (Alta p)  = mapM_ putStrLn p
listUrls (Media p) = mapM_ putStrLn p
listUrls (Baja p)  = mapM_ putStrLn p


checkUrl :: Url -> IO ()
checkUrl s = do 
                x <- try ( simpleHTTP (getRequest s) ) :: IO (Either SomeException (Result (Response String) ) )
                case x of
                     Left ex   -> putStrLn $ "La pagina  "++s++" se encuentra: ONLINE "
                     Right val -> putStrLn $ "La pagina  "++s++" se encuentra: OFFLINE" 


