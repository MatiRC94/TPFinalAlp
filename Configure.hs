module Configure where

import System.Console.ANSI as A
import System.IO
import System.Directory
import Data.Char (digitToInt)
--import Data.Text.IO (putStrLn)
import Parsing 
import Dataparalelo as D
import TinyParser

initialConfig = "#P [] [] [] \n#Fondo 0 0 \n#Fuente 7 1"

cfg = "Config/Config.cfg"
--fpurl = "Config/Urls.cfg"

-- TODO funcion estilo de dataparalelo aca, que modifique Config.cfg
-- TODO add y remove url debe estar aca y modificar url.cfg
--- ACA ESTA TODO LO QUE VENGA EN RELACION A LOS ARCHIVOS.CFG



         



evalConf :: Config -> IO ()
evalConf (Fondo c i)  = setSGR [ SetColor Background (toColorI i) (toColor c) ]
evalConf (Fuente c i) = setSGR [ SetColor Foreground (toColorI i) (toColor c) ]


checkCfg :: IO ()
checkCfg = do 
             bool <- doesFileExist cfg
             if bool then return () else defaultConfig
 


defaultConfig :: IO ()
defaultConfig = writeFile cfg initialConfig

procesarConf :: String -> IO Prior --([Config],Prior)
procesarConf cont = do
                      mapM_ evalConf $ fst c
                      return (snd c)
                      where c = fst $ (parse (p1 [] (D.P [] [] []) ) cont ) !! 0 

--procesarConf :: String -> IO ()

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
