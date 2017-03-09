
module Dataparalelo where

import Network.HTTP (getRequest,simpleHTTP,Response)
import Network.Stream (Result)
import Control.Exception (try,SomeException)
import Data.List (delete)


data Priority = Alta | Media | Baja deriving Show

data Prior = P { a :: [Url]
                ,m :: [Url]
                ,b :: [Url] } deriving Show

type Url = String


addUrl2 :: Url -> Priority -> Prior -> Prior
addUrl2 s Alta (P a m b)  = P (s:a) m b
addUrl2 s Media (P a m b) = P a (s:m) b
addUrl2 s Baja (P a m b)  = P a m (s:b)


removeUrl :: Url -> Prior -> Prior
removeUrl s (P a m b)  = P (delete s a) (delete s m) (delete s b)


listUrls :: Prior -> IO ()
listUrls p = do
               putStrLn "Urls de prioridad Alta:  "
               mapM_ putStrLn ( a p )
               putStrLn "Urls de prioridad Media: "
               mapM_ putStrLn ( m p )
               putStrLn "Urls de prioridad Baja:  "
               mapM_ putStrLn ( b p )



checkUrl :: Url -> IO String
checkUrl s = do 
                x <- try ( simpleHTTP (getRequest s) ) :: IO (Either SomeException (Result (Response String) ) )
                case x of
                     Left ex   -> return $ "OFFLINE"
                     Right val -> return $ "ONLINE"


initial = P [] [] []

caca = addUrl2 "1" Alta ( addUrl2 "3" Baja ( addUrl2 "11" Alta ( addUrl2 "2" Media ( initial) ) ) )
