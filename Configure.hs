module Configure where

import System.Console.ANSI as A
import System.IO
import System.Directory
import Data.Char (digitToInt)
--import Data.Text.IO (putStrLn)
import Parsing 
import Dataparalelo as D
import TinyParser


initialConfig = "#Fondo 4 0 \n#Fuente 3 1 \n#P [] [] []"


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

restoreDefault :: IO ()
restoreDefault = do
                    defaultConfig
                    cont <- readFile cfg
                    procesarConf 
                    clearScreen
                    return ()                    

procesarConf :: IO ([Config],Prior)
procesarConf = do
                  cont <- readFile cfg
                  mapM_ evalConf $ fst (fst $ (parse (p1 [] (D.P [] [] []) ) cont ) !! 0 )
                  return (fst (fst ( (parse (p1 [] (D.P [] [] []) ) cont ) !! 0))  ,(snd (fst $ (parse (p1 [] (D.P [] [] []) ) cont ) !! 0 )) )
                --  where c = fst $ (parse (p1 [] (D.P [] [] []) ) cont ) !! 0 

--procesarConf :: String -> IO ()

estilo :: Prior -> IO ()
estilo p = do
              putStrLn "Elija su estilo "
              elegirColor p

elegirColor :: Prior -> IO ()
elegirColor p = do
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
                 --   setSGR [SetColor Foreground (toColorI (digitToInt(i2)) ) (toColor (digitToInt(c2)) ), SetColor Background (toColorI (digitToInt(i1))) (toColor (digitToInt(c1)))]
                     changeConfigCol p [digitToInt(c1),digitToInt(i1),digitToInt(c2),digitToInt(i2)] 

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


changeConfigCol :: Prior -> [Int] -> IO ()
changeConfigCol (D.P a m b) c = do
                                  writeFile cfg $ "#Fondo "++ show (c!!0) ++" "++ show (c!!1) ++"\n"++"#Fuente "++ show (c!!2) ++" "++ show (c!!3) ++"\n"++"#P["++auxurl a ++"] ["++auxurl m ++"] ["++auxurl b++"]"
                                  procesarConf
                                  return ()


agregarUrlConf :: Url -> Priority -> Prior -> [Config] -> IO () 
agregarUrlConf url p pr conf = do
                                 writeFile cfg $ "#"++show (conf!!0) ++"\n"++"#"++ show (conf!!1) ++"\n"++"#P ["++auxurl(a newUrl) ++"] ["++auxurl (m newUrl)++"] ["++auxurl (b newUrl)++"]"
                                 procesarConf
                                 return ()
                                 where newUrl = (addUrl url p pr)



aux10 :: [Url] -> [Url]
aux10 []     = []
aux10 [x]    = [x]
aux10 (x:xs) = (x++","): (aux10 xs)
 
auxurl :: [Url] -> [Char]
auxurl u = concat (aux10 u)





