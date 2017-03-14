
module Dataparalelo where

import Network.HTTP (getRequest,simpleHTTP,Response)
import Network.Stream (Result)
import Control.Exception (try,SomeException)
import Data.List (delete,union)
import System.Console.ANSI as A
import Data.Char (digitToInt)

data Priority = Alta | Media | Baja deriving Show

data Prior = P { a :: [Url]
                ,m :: [Url]
                ,b :: [Url] } deriving Show


data Cfg = C { colorFondo :: SGR
               ,colorFuente :: SGR } deriving Show

data Config = Fondo Int Int |
              Fuente Int Int deriving Show

--data Prio = Pr [Url] [Url] [Url] deriving Show

type Url = String

--TODO LO RELACIONADO A LA DEFINICION DE DATOS Y ESTRUCT

colores =["0-Negro","1-Rojo","2-Verde","3-Amarillo","4-Azul","5-Magenta","6-Cyan","7-Blanco"]
intensidad = ["0-Opaco","1-Vivido"]


addUrl :: Url -> Priority -> Prior -> Prior
addUrl s Alta (P a m b)  = P (union [s] a) m b
addUrl s Media (P a m b) = P a (union [s] m) b
addUrl s Baja (P a m b)  = P a m (union [s] b)


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


