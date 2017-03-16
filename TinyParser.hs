module TinyParser where

import Parsing 
import Dataparalelo as D




fond :: Parser Config
fond = do
         symbol "Fondo"
         space
         c <- nat
         space
         i <- nat
         return $ Fondo c i


font :: Parser Config
font = do
         symbol "Fuente"
         space
         c <- nat
         space
         i <- nat
         return $ Fuente c i

               
 {-                  
conf  :: Parser Config
conf  = (do fondo  <- fond
            return fondo)
         <|> do fuente <- font 
                return $ fuente         
-}

com1 :: Parser Char
com1 = do
         alphanum <|> (char '-') <|> (char '.') <|> (char '/')                      


urlP :: Parser Prior
urlP =  do
          symbol "P"
          space
          char '['
   --       char '"'
          alta <- urlParse
          char ']'
          space
          char '['
   --       char '"'
          media <- urlParse
          char ']'
          space
          char '['
  --        char '"'
          baja <- urlParse
          char ']'
          return $ D.P alta media baja


urlParse :: Parser [Url]
urlParse = do
             sepBy ( aux2 ) (char ',') 

aux2 :: Parser String
aux2 = many1((do 
                x <- alphanum
                return x )
             <|> do p <- char '.'
                    return p
             <|> do p <- char '/'
                    return p
             <|> do p <- char ':'
                    return p
             <|> do p <- char '_'
                    return p
             <|> do p <- char '-'
                    return p )

aux3 :: Parser ()
aux3 = do
         char '"'
         char ','
         char '"'
         space
       <|> do
             char '"'
             space
       <|> space
{-                         
num# -> # n | comment
n -> P [algo] [algo] [algo n| Fuente i i n| Fondo i i n| space  
algo -> '"' string '"' ( ']' | ',' ) 
space -> \n comment | '_' space|  
comment -> comentariosyeso num#
-}

p1 :: [Config] -> Prior -> Parser ([Config],Prior)
p1 config (D.P a m b)  = (do
                            space
                            char '#' 
                            space
                            (do 
                               prior <- urlP
                               p1 config prior
                               <|> do
                                     fondo <- fond
                                     p1 (reemp fondo config) (D.P a m b)
                               <|> do
                                     fuente <- font
                                     p1 (reemp fuente config) (D.P a m b) ) )
                            <|> do 
                                  space
                                  com1
                                  space
                                  p1 config (D.P a m b)
                            <|> return (config,(D.P a m b))
                                   


reemp :: Config -> [Config] -> [Config]
reemp c [] = [c]
reemp (Fondo a b) (x:xs) = case x of
                                Fondo p t -> (Fondo a b):xs
                                _         -> x : (reemp (Fondo a b) xs)
reemp (Fuente a b) (x:xs) = case x of
                                Fuente p t -> (Fuente a b):xs
                                _          -> x : (reemp (Fuente a b) xs)



--parse (p1 [] (D.P [] [] [])) "#P [] ["http://www.lacapital.com","http://www.rosario3.com"] [] caca \n caca \n #Fondo 0 0 \n #Fuente 7 1 caca"

--parse (p1 [] (D.P [] [] [])) "#Fondo 4 0 \n#Fuente 3 1\n#P [http://www.clarin.com/,http://www.lacapital.com] [] []"







