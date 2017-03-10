module TinyParser where

import Parsing --(parse, Parser, char, symbol, space, nat,(<|>),endBy)
import Dataparalelo as D



fond :: Parser Config
fond = do
         char '#'
         symbol "Fondo"
         space
         c <- nat
         space
         i <- nat
         return $ Fondo c i


font :: Parser Config
font = do
         char '#'
         symbol "Fuente"
         space
         c <- nat
         space
         i <- nat
         return $ Fuente c i

               
                   
prueba :: Parser Config
prueba = (do fondo  <- fond
             return fondo)
          <|> do fuente <- font 
                 return $ fuente         



comment :: Parser [Config]
comment = do
            endBy prueba (do 
                           space
                           sepBy com1 space
                           space)

com1 :: Parser Char
com1 = do
         alphanum <|> (char '-') <|> (char '.')  <|> (char '/')



url :: Parser Prior
url =  do
         char '#'
         symbol "P"
         space
         char '['
         alta <- aux
         char ']'
         space
         char '['
         media <- aux
         char ']'
         space
         char '['
         baja <- aux
         char ']'
         return $ D.P alta media baja


aux :: Parser [Url]
aux = do
        sepBy (identifier <|> aux2) (char ',') 

aux2 :: Parser String
aux2 = do
        p <- string "."
        return p
      <|> 
     do b <- string "/"
        return b

