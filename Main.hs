{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import System.Console.ANSI as A
import System.IO
import System.Console.Terminal.Size 
import Data.Maybe (fromJust)
import Data.Char (digitToInt)

colores =["0-Negro","1-Rojo","2-Verde","3-Amarillo","4-Azul","5-Magenta","6-Cyan","7-Blanco"]
intensidad = ["0-Opaco","1-Vivido"]


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
                putStr  "\n"
                mapM_ putStrLn intensidad
                getChar
     
toColor:: Int -> Color
toColor = toEnum

toColorI:: Int -> ColorIntensity
toColorI = toEnum

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
    setTitle "Resumidor de Noticias"
    t <- cursorCol
    setCursorPosition 0 t
    putStr "Bienvenido al Resumidor de noticias"
    setCursorPosition 5 0
    elegirColor
    clearScreen
    setCursorPosition 0 0







