module Dataparalelo where

import Network.HTTP (getRequest,simpleHTTP,Response)
import Network.Stream (Result)
import Control.Exception (try,SomeException)
import Data.List (delete,union)
import System.Console.ANSI as A
import Data.Char (digitToInt)


type Url = String

data Priority = Alta | Media | Baja deriving Show

data Prior = P { a :: [Url]
                ,m :: [Url]
                ,b :: [Url] } deriving Show

data Config = Fondo Int Int |
              Fuente Int Int deriving Show

data News = N {  na :: ([(String,Url)],Int)
                ,nm :: ([(String,Url)],Int)
                ,nb :: ([(String,Url)],Int) } deriving Show




addUrl' :: Url -> Priority -> Prior -> IO Prior
addUrl' s Alta (P a m b)  = return $ P (union [s] a) m b
addUrl' s Media (P a m b) = return $ P a (union [s] m) b
addUrl' s Baja (P a m b)  = return $ P a m (union [s] b)

addUrl :: Url -> Priority -> Prior -> IO Prior
addUrl s Alta (P a m b) =  removeUrl s (P a m b) >>= addUrl' s Alta
addUrl s Media (P a m b) = removeUrl s (P a m b) >>= addUrl' s Media 
addUrl s Baja (P a m b) =  removeUrl s (P a m b) >>= addUrl' s Baja 

removeUrl :: Url -> Prior -> IO Prior
removeUrl s (P a m b)  = return $ P (delete s a) (delete s m) (delete s b)


showUrls :: Prior -> IO ()
showUrls p = do
               putStrLn "Rss de prioridad Alta:  "
               mapM_ putStrLn ( a p )
               putStrLn "Rss de prioridad Media: "
               mapM_ putStrLn ( m p )
               putStrLn "Rss de prioridad Baja:  "
               mapM_ putStrLn ( b p )

checkUrl :: Url -> IO ()
checkUrl s = do 
                x <- try ( simpleHTTP (getRequest s) ) :: IO (Either SomeException (Result (Response String) ) )
                case x of
                     Left ex   -> putStrLn "OFFLINE y cuidado que no funciona con https"
                     Right val -> putStrLn "ONLINE"

checkAll' :: [Url] -> IO ()
checkAll' url = mapM_ checkUrl url

checkAll :: Prior -> IO ()
checkAll (P a m b) = do
                       checkAll' a
                       checkAll' m
                       checkAll' b  

getUrlNews :: Priority -> News -> Int -> Url
getUrlNews Alta (N na nm nb) n  = if snd na < n+1 || n < 0 then "Error de Indice" else snd $ (fst na)!!n
getUrlNews Media (N na nm nb) n = if snd nm < n+1 || n < 0 then "Error de Indice" else snd $ (fst nm)!!n
getUrlNews Baja (N na nm nb) n  = if snd nb < n+1 || n < 0 then "Error de Indice" else snd $ (fst nb)!!n





