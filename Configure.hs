module Configure where

import System.Console.ANSI as A
import System.IO

import Data.Char (digitToInt)
--import Data.Text.IO (putStrLn)
import Dataparalelo


fpc = "Config/Config.cfg"
fpurl = "Config/Urls.cfg"

-- TODO funcion estilo de dataparalelo aca, que modifique Config.cfg
-- TODO add y remove url debe estar aca y modificar url.cfg


main :: IO ()
main = do 
         --hconf <- openFile fpc ReadWriteMode
         --content <- hGetContents hconf 
         content <- readFile fpc
         putStrLn content
         writeFile fpc "pedos"
       


evalConf :: Config -> IO ()
evalConf (Fondo c i)  = setSGR [ SetColor Background (toColorI i) (toColor c) ]
evalConf (Fuente c i) = setSGR [ SetColor Foreground (toColorI i) (toColor c) ]



--procesarConf :: String -> IO ()

