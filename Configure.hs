module Configure where

import Text.Show.Unicode(ushow)
import System.Console.ANSI as A
import System.IO
import System.Directory
import Data.Char (digitToInt)
--import Data.Text.IO (putStrLn)
import Parsing 
import Dataparalelo as D
import TinyParser
import Scraper

initialNews = "# NA ([],0) \n# NM ([],0) \n# NB ([],0)"
initialConfig = "#Fondo 4 0 \n#Fuente 3 1 \n#P [] [] []"
emptyData = D.P [] [] []
emptyNews = (N ([],0) ([],0) ([],0))

cfg = "Config/Config.cfg"
notis="Config/Noticias.cfg"

-- TODO funcion estilo de dataparalelo aca, que modifique Config.cfg
-- TODO add y remove url debe estar aca y modificar url.cfg
--- ACA ESTA TODO LO QUE VENGA EN RELACION A LOS ARCHIVOS.CFG


evalConf :: Config -> IO ()
evalConf (Fondo c i)  = setSGR [ SetColor Background (toColorI i) (toColor c) ]
evalConf (Fuente c i) = setSGR [ SetColor Foreground (toColorI i) (toColor c) ]

findNews  :: IO News
findNews  = do
              checkNews
              contN <- readFile notis           
              return $ fst $ (parse (parseNews emptyNews) contN) !! 0

checkNews :: IO ()
checkNews = do 
              bool <- doesFileExist notis
              if bool then return () else defaultNews

checkCfg :: IO ()
checkCfg = do 
             bool <- doesFileExist cfg
             if bool then return () else defaultConfig

defaultNews :: IO ()
defaultNews = do 
                writeFile notis initialNews

defaultConfig :: IO ()
defaultConfig = do 
                  writeFile cfg initialConfig
                  writeFile notis initialNews

restoreDefault :: IO ()
restoreDefault = do
                    defaultConfig
                    defaultNews
                    cont <- readFile cfg
                    procesarConf 
                    clearScreen
                    return ()                    

procesarConf :: IO ([Config],Prior)
procesarConf = do
                  cont <- readFile cfg
                  let c = (fst $ (parse (p1 [] emptyData ) cont ) !! 0 )
                      in do mapM_ evalConf $ fst c
                            return (fst c ,snd c)       

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
                                  writeFile cfg $ "#Fondo "++ show (c!!0) ++" "++ show (c!!1) ++"\n"++"#Fuente "++ show (c!!2) ++" "++ show (c!!3) ++"\n"++"#P "++show a  ++ show m ++show b
                                  procesarConf
                                  return ()

agregarUrlConf :: Url -> Priority -> Prior -> [Config] -> IO () 
agregarUrlConf url p pr conf = do
                                 writeFile cfg $ "#"++show (conf!!0) ++"\n"++"#"++ show (conf!!1) ++"\n"++"#P "++show(a newUrl) ++ show (m newUrl)++show (b newUrl)
                                 procesarConf
                                 return ()
                                 where newUrl = (addUrl url p pr)

showNews :: Priority -> IO ()
showNews Alta  = findNews >>= \x -> mapM_ auxPrint (fst $ na x)
showNews Media = findNews >>= \x -> mapM_ auxPrint (fst $ nm x)
showNews Baja  = findNews >>= \x -> mapM_ auxPrint (fst $ nb x)
--([(String,Url)],Int)
auxPrint :: (String,Url) -> IO ()
auxPrint n = putStrLn $ fst n

showNewsLink :: Priority -> IO ()
showNewsLink Alta  = findNews >>= \x -> mapM_ auxPrint2 (fst $ na x)
showNewsLink Media = findNews >>= \x -> mapM_ auxPrint2 (fst $ nm x)
showNewsLink Baja  = findNews >>= \x -> mapM_ auxPrint2 (fst $ nb x)

auxPrint2 :: (String,Url) -> IO ()
auxPrint2 n = putStrLn $ snd n
-- A partir de una Prioridad y una lista, Actualizo las noticias en el archivo de noticias 
updateNews :: Priority -> Prior -> News -> IO ()
updateNews Alta p n = let newslist = (a p)                          
                          in do 
                               if (length newslist == 0) then putStrLn "No hay ningun diario en la lista." else do {scrapeo <- auxParse newslist ; writeNews Alta scrapeo p n}
updateNews Media p n= let newslist = (m p)                          
                          in do 
                               if (length newslist == 0) then putStrLn "No hay ningun diario en la lista." else do {scrapeo <- auxParse newslist ; writeNews Media scrapeo p n}
updateNews Baja p n = let newslist = (b p)                          
                          in do 
                               if (length newslist == 0) then putStrLn "No hay ningun diario en la lista." else do {scrapeo <- auxParse newslist ; writeNews Baja scrapeo p n}

auxParse :: [Url] -> IO [(String, Url)]
auxParse []   = return []  
auxParse (x:xs) =  do 
                     scr  <- scrap x
                     scr2 <- auxParse xs
                     return $ scr++scr2

writeNews :: Priority -> [(String,Url)] -> Prior -> News -> IO ()
writeNews Alta l p (N na1 nm1 nb1) = do
                                        let tam = length l
                                        writeFile notis $ "# NA ("++ ushow l ++","++ ushow tam ++")"++"\n# NM "++ ushow nm1 ++"\n# NB "++ ushow nb1
writeNews Media l p (N na1 nm1 nb1)= do
                                        let tam = length l
                                        writeFile notis $ "# NA "++ ushow na1 ++ "\n# NM ("++ ushow l ++","++ ushow tam ++")"++"\n# NB "++ ushow nb1
writeNews Baja l p (N na1 nm1 nb1) = do
                                        let tam = length l
                                        writeFile notis $ "# NA "++ ushow na1 ++ "\n# NM "++ ushow nm1 ++"\n# NB ("++ ushow l ++","++ ushow tam ++")"





