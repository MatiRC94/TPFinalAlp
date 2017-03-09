{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import System.Console.ANSI as A
import System.IO
import System.Console.Terminal.Size 
import Data.Maybe (fromJust)
import Dataparalelo



initial = P [] [] []

caca = addUrl "1" Alta ( addUrl "3" Baja ( addUrl "11" Alta ( addUrl "2" Media ( initial) ) ) )




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







