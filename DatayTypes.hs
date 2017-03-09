
module DatayTypes where

import Network.HTTP (getRequest,simpleHTTP,Response)
import Network.Stream (Result)
import Control.Exception (try,SomeException)
import Data.List (delete)


data Priority = Alta [Url] | Media [Url] | Baja [Url] deriving Show

data Prior = P { a :: [Url]
                ,m :: [Url]
                ,b :: [Url] } deriving Show

type Url = String

addUrl :: Url -> Priority -> Priority
addUrl s (Alta p)  = Alta (s : p)
addUrl s (Media p)  = Media (s : p)
addUrl s (Baja p)  = Baja (s : p)



removeUrl :: Url -> Priority -> Priority
removeUrl s (Alta p)  = Alta $ delete s p
removeUrl s (Media p) = Media $ delete s p
removeUrl s (Baja p)  = Baja $ delete s p



listUrls :: Priority -> IO ()
listUrls (Alta p)  = mapM_ putStrLn p
listUrls (Media p) = mapM_ putStrLn p
listUrls (Baja p)  = mapM_ putStrLn p


checkUrl :: Url -> IO String
checkUrl s = do 
                x <- try ( simpleHTTP (getRequest s) ) :: IO (Either SomeException (Result (Response String) ) )
                case x of
                     Left ex   -> return $ "OFFLINE"
                     Right val -> return $ "ONLINE"


