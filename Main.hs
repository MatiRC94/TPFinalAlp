module Main where

import System.Console.ANSI as A 
import System.IO (hSetBuffering, stdin, BufferMode(LineBuffering))
import System.Console.Terminal.Size -- (size, Window)
import System.Exit (exitSuccess) 
import Data.Char (digitToInt)
import Data.Maybe (fromJust)

import Dataparalelo
import Configure

opciones = ["1- Ver Noticias", "2- Agregar Links RSS", "3- Info RSS", "4- Opciones Graficas", "0- Salir","\n\nCualquier Otra tecla es Erronea"]
opRss = ["1- Ver Status Rss", "2- Eliminar RSS", "3- Agregar Links RSS","0- Salir"]
prioridad = ["1- Alta", "2- Media", "3- Baja", "0- Salir"]
titulo = "Resumidor de Noticias"
bienvenida = "Bienvenido al Visor de noticias"


--TODO VER Q PASA SI AGREGO UNA URL INCORRECTA Y COMO MANEJARLO
--TODO Bajar subir con flechitas
-- TODO SACAR URL removerUrlConf  en Configure.hs


--Funcion para cargar el buffer y poder aceptar backspaces con getLine
buffering :: IO ()
buffering = hSetBuffering stdin LineBuffering


tamano2 :: IO (Window Int)
tamano2 = size >>= \x -> return (fromJust x)

tamano :: IO Int
tamano = tamano2 >>= \x -> case x of
                                Window a b -> return b

-- Centrar el cursor para Escribir el titulo del Programa
cursorCol :: IO Int
cursorCol = tamano >>= \t -> return (div (t- length bienvenida) 2)            

cursorStart :: IO ()
cursorStart =  clearScreen >> setCursorPosition 0 0

menu :: ([Config],Prior) -> IO ()           
menu tup = do
             putStrLn "Elija alguna Accion\n"
             c <- listarOpc opciones
             case c of
                  '1' -> findNews >>= updateNews Alta (snd tup) >>verNoticias
                  '2' -> agregarLinks tup
                  '3' -> infoRss (snd tup) (fst tup)
                  '4' -> putStrLn "4"
                  '0' -> exitSuccess
                  _   -> putStrLn "\nTecla incorrecta"
             tup2 <- procesarConf
             cursorStart
             menu tup2

volverMenu :: IO ()
volverMenu = do
                        putStrLn "\n"
                        putStrLn "\n\nPresione Cualquier tecla para volver al menu principal\n0- Salir "
                        noBuffering
                        c <- getChar
                        case c of
--                            'w' -> scrollPageUp 1 >> volverMenu
  --                          's' -> scrollPageDown 1 >> volverMenu
                            '0' -> exitSuccess
                            _   -> return ()

agregarLinks :: ([Config],Prior) -> IO ()
agregarLinks tup = do 
                     buffering
                     cursorStart
                     putStrLn "Ingrese Link RSS:"
                     url <- getLine
                     cursorStart
                     putStrLn "Prioridad ?"                    
                     c <- listarOpc prioridad
                     putStr "\n"                     
                     case c of
                         '1' -> agregarUrlConf url Alta tup >> putStrLn  (url++" Agregado") >> volverMenu
                         '2' -> agregarUrlConf url Media tup >> putStrLn (url++" Agregado") >> volverMenu
                         '3' -> agregarUrlConf url Baja tup >> putStrLn (url++" Agregado") >> volverMenu
                         '0' -> exitSuccess
                         _   -> putStrLn "Tecla incorrecta"

verNoticias :: IO ()
verNoticias = do 
                 cursorStart
                 putStrLn "Noticias de que prioridad desea ver?"                    
                 c <- listarOpc prioridad
                 cursorStart
                 case c of
                     '1' -> showNews Alta >> volverMenu
                     '2' -> showNews Media >> volverMenu
                     '3' -> showNews Baja >> volverMenu
                     '0' -> exitSuccess
                     _   -> putStrLn "Tecla incorrecta"

infoRss :: Prior -> [Config] -> IO ()
infoRss pr conf = do
                    cursorStart
                    putStrLn "Que desea hacer ?"
                    c <- listarOpc opRss
                    case c of
                         '1' -> cursorStart >> showUrls pr >> checkAll pr >> volverMenu
                         '2' -> cursorStart >> showUrls pr >> eliminarRss pr conf >> volverMenu
                         '3' -> agregarLinks (conf,pr) >> volverMenu
                         '0' -> exitSuccess
                         _   -> putStrLn "Tecla incorrecta"

eliminarRss :: Prior -> [Config] -> IO ()
eliminarRss pr conf = putStrLn "Ingrese Link RSS:" >> 
                      buffering >> 
                      getLine >>= \url -> removerUrlConf url (conf,pr) >>   
                      if elem url (a pr) || elem url (m pr) || elem url (b pr) then putStrLn $ url++"    Removido de la lista " else putStrLn "Url Invalida" >> eliminarRss pr conf



main :: IO ()
main = do
        noBuffering
        checkCfg
        info <- procesarConf
        clearScreen
        setTitle titulo
        t <- cursorCol
        setCursorPosition 0 t
        putStr bienvenida
        setCursorPosition 5 0
        menu info
        --content <- readFile cfg
{-
        info <- procesarConf
        clearScreen
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
        do info <- procesarConf     ; agregarUrlConf "http://www.ole.com.ar/rss/ultimas-noticias/" Alta info
    do info <- procesarConf     ; agregarUrlConf "http://www.clarin.com/rss/lo-ultimo/" Alta info
    do info <- procesarConf     ;  showUrls $ snd info
    updateNews :: Priority -> Prior -> News -> IO ()
    do info <- procesarConf     ; new <- findNews ;  updateNews Alta (snd info) new :: IO ()

-}
         
         


