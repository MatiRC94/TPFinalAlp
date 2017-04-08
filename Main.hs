module Main where

import System.Console.ANSI as A 
import System.IO (hSetBuffering, stdin, BufferMode(LineBuffering))
import System.Console.Terminal.Size -- (size, Window)
import System.Exit (exitSuccess) 
import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import System.Process (runCommand, ProcessHandle)

import Parsing (parse, integer)
import Dataparalelo
import Configure

opciones = ["1- Ver Noticias", "2- Agregar Links RSS", "3- Info RSS", "4- Opciones Graficas", "q- Salir","\n\nCualquier Otra tecla es Erronea"]
opRss = ["1- Ver Status Rss", "2- Eliminar RSS", "3- Agregar Links RSS","q- Salir"]
prioridad = ["1- Alta", "2- Media", "3- Baja", "q- Salir"]
titulo = "Resumidor de Noticias"
bienvenida = "Bienvenido al Visor de noticias"


--TODO VER Q PASA SI AGREGO UNA URL INCORRECTA Y COMO MANEJARLO
--TODO LA PRIMERA VEZ HACE ERROR DE LINK
--TODO PONER CARTEL DE CARGANDO CUANDO UPDATEAS PORQUE TARDA!!!!
--TODO Opciones graficas
--TODO La parte de opcioines para abrir en firefox



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
                  '1' -> findNews >>= verNoticias (snd tup)
                  '2' -> agregarLinks tup
                  '3' -> infoRss (snd tup) (fst tup)
                  '4' -> putStrLn "4"
                  'q' -> exitSuccess
                  _   -> putStrLn "\nTecla incorrecta"
             tup2 <- procesarConf
             cursorStart
             menu tup2

volverMenu :: IO ()
volverMenu = do
                        putStrLn "\n"
                        putStrLn "\n\nPresione Cualquier tecla para volver al menu principal\nq- Salir "
                        noBuffering
                        c <- getChar
                        case c of
--                            'w' -> scrollPageUp 1 >> volverMenu
  --                          's' -> scrollPageDown 1 >> volverMenu
                            'q' -> exitSuccess
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
                         'q' -> exitSuccess
                         _   -> putStrLn "Tecla incorrecta"

verNoticias :: Prior-> News -> IO ()
verNoticias p n = do 
                     cursorStart
                     putStrLn "Noticias de que prioridad desea ver?"                    
                     c <- listarOpc prioridad
                     cursorStart
                     case c of
                         '1' -> updateNews Alta p n >>= \x -> case x of
                                                                   1 -> volverMenu
                                                                   0 -> showNews Alta >> irUrl Alta n >> volverMenu
                         '2' -> updateNews Media p n >>= \x -> case x of
                                                                    1 -> volverMenu
                                                                    0 -> showNews Media >> irUrl Media n >> volverMenu
                         '3' -> updateNews Baja p n >>= \x -> case x of
                                                                   1 -> volverMenu
                                                                   0 -> showNews Baja >> irUrl Baja n >> volverMenu
                         'q' -> exitSuccess
                         _   -> putStrLn "Tecla incorrecta"

irUrl :: Priority -> News -> IO ProcessHandle
irUrl p n = do
                 putStrLn "\n"
                 putStrLn "\n\nElija una Noticia para Abrir en Firefox"
                 buffering
                 c <- getLine
                 case tinyParser c of
                      Left x -> irUrl p n
                      Right i -> runCommand $ "firefox "++getUrlNews p n i
                 
                


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
                      getLine >>= \url -> if elem url (a pr) || elem url (m pr) || elem url (b pr) then removerUrlConf url (conf,pr) >>  putStrLn ( url++"    Removido de la lista ") else putStrLn "Url Invalida" >> eliminarRss pr conf
                      
  

tinyParser :: String -> Either (IO ()) Int
tinyParser s = case (parse integer s) of
                    [] -> Left $ putStrLn (s++"No es una opcion valida")
                    x  -> case (x!!0) of
                               (a,"") -> Right a
                               _      -> Left $ putStrLn (s++"No es una opcion valida")





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
    do info <- procesarConf     ; new <- findNews ;  updateNews Media (snd info) new :: IO ()

-}
         
         


