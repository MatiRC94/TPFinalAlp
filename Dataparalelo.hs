
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


type Url = String

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



checkUrl :: Url -> IO String
checkUrl s = do 
                x <- try ( simpleHTTP (getRequest s) ) :: IO (Either SomeException (Result (Response String) ) )
                case x of
                     Left ex   -> return $ "OFFLINE"
                     Right val -> return $ "ONLINE"




elegirColor :: IO ()
elegirColor = do
            putStrLn "Elija su estilo "
            estilo


estilo :: IO ()
estilo = do
        putStrLn "Color de Fondo:"
        c1  <- listaColores
        putStr  "\n"
        putStrLn "Intensidad del color:"
        i1 <- intenSidad
        putStr  "\n"
        putStrLn "Color de Fuente:"
        c2  <- listaColores
        putStr  "\n"
        putStrLn "Intensidad del color:"
        i2 <- intenSidad
        putStr  "\n" 
        setSGR [SetColor Foreground (toColorI (digitToInt(i2)) ) (toColor (digitToInt(c2)) ), SetColor Background (toColorI (digitToInt(i1))) (toColor (digitToInt(c1)))]

listaColores :: IO Char
listaColores = do 
                mapM_ putStrLn colores
                putStr  "\n"
                getChar


intenSidad :: IO Char
intenSidad = do
                mapM_ putStrLn intensidad
                putStr  "\n"
                getChar
     
toColor:: Int -> Color
toColor = toEnum

toColorI:: Int -> ColorIntensity
toColorI = toEnum
