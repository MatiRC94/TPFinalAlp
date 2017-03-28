module Main where

import System.Console.ANSI as A 
import System.IO 
import System.Console.Terminal.Size -- (size, Window)
import Data.Char (digitToInt)
import Data.Maybe (fromJust)

import Dataparalelo
import Configure

opciones = ["1- Ver Noticias", "2- Agregar Links RSS", "3- Opciones", "4- Salir"]
prioridad = ["1- Alta", "2- Media", "3- Baja"]

tamano2 :: IO (Window Int)
tamano2 = size >>= \x -> return (fromJust x)

tamano :: IO Int
tamano = tamano2 >>= \x -> case x of
                                Window a b -> return b
        
cursorCol :: IO Int
cursorCol = tamano >>= \t -> return (div (t-35) 2)            


menu :: ([Config],Prior) -> IO ()           
menu tup = do
             putStrLn "Elija alguna Accion\n"
             c <- listarOpc opciones
             clearScreen
             case c of
                  '1' -> putStrLn "Su eleccion fue 1"
                  '2' -> agregarLinks tup
                  '3' -> putStrLn "3"
                  '4' -> putStrLn "4"

agregarLinks :: ([Config],Prior) -> IO()
agregarLinks tup = do 
                     putStrLn "Ingrese Link RSS:"
                     url <- getLine
                     putStrLn "Prioridad ?"                    
                     c <- listarOpc prioridad
                     case c of
                         '1' -> agregarUrlConf url Alta tup >>=  \x -> putStrLn  (url++" Agregado")
                         '2' -> agregarUrlConf url Media tup >>= \x -> putStrLn (url++" Agregado")
                         '3' -> agregarUrlConf url Baja tup >>=  \x -> putStrLn (url++" Agregado")



main :: IO ()
main = do
        clearScreen
        checkCfg
        info <- procesarConf
        menu info
        --content <- readFile cfg
{-
        info <- procesarConf
        clearScreen
        setTitle "Resumidor de Noticias"
        t <- cursorCol
        setCursorPosition 0 t
        putStr "Bienvenido al Resumidor de noticias"
        setCursorPosition 5 0
        showUrls $ snd info
        checkAll $ snd info
        -- Menu inicial
-}



{-
   procesarConf :: IO ([Config],Prior)
 --   estilo $ snd info
--    agregarUrlConf "http://www.lacapital.com" Alta (snd info) (fst info)
--    agregarUrlConf "http://www.clarin.com/" Alta (snd info) (fst info)
--    url2 <- addUrl "http://www.clarin.com/" Alta urls
    0elegirColor
    clearScreen
    setCursorPosition 0 0
    do info <- procesarConf     ; agregarUrlConf "http://www.ole.com.ar/rss/ultimas-noticias/" Alta (snd info) (fst info) 
    do info <- procesarConf     ; agregarUrlConf "http://www.clarin.com/rss/lo-ultimo/" Alta (snd info) (fst info) 
    do info <- procesarConf     ;  showUrls $ snd info
    do info <- procesarConf     ;  agregarUrlConf "http://www.clarin.com/" Alta (snd info) (fst info)
    updateNews :: Priority -> Prior -> News -> IO ()
    do info <- procesarConf     ; new <- findNews ;  updateNews Alta (snd info) new :: IO ()

-}
         
         


