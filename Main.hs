{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import System.Console.ANSI as A
import System.IO
import System.Console.Terminal.Size 
import Data.Maybe (fromJust)
import Dataparalelo
import Configure






{-
backColor :: Int -> Int -> SGR
backColor c i = SetColor Background (toColorI (i)) (toColor (c))
 -}


tamano2 :: IO (Window Int)
tamano2 = size >>= \x -> return (fromJust x)

tamano :: IO Int
tamano = tamano2 >>= \x -> case x of
                                Window a b -> return b
        
cursorCol :: IO Int
cursorCol = tamano >>= \t -> return (div (t-35) 2)            
            


main :: IO ()
main = do
    clearScreen
    checkCfg
    --content <- readFile cfg
    info <- procesarConf
    clearScreen
    setTitle "Resumidor de Noticias"
    t <- cursorCol
    setCursorPosition 0 t
    putStr "Bienvenido al Resumidor de noticias"
    setCursorPosition 5 0
    listUrls $ snd info
    checkAll $ snd info
    -- Menu inicial




{-
 --   estilo $ snd info
--    agregarUrlConf "http://www.lacapital.com" Alta (snd info) (fst info)
--    agregarUrlConf "http://www.clarin.com/" Alta (snd info) (fst info)
--    url2 <- addUrl "http://www.clarin.com/" Alta urls
    0elegirColor
    clearScreen
    setCursorPosition 0 0
    do info <- procesarConf     ; agregarUrlConf "http://www.lacapital.com" Alta (snd info) (fst info) 
    do info <- procesarConf     ;    listUrls $ snd info
    do info <- procesarConf     ;  agregarUrlConf "http://www.clarin.com/" Alta (snd info) (fst info)

-}
         
         


