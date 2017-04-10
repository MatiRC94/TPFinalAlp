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

--Parser de Prior
urlP :: Parser Prior
urlP =  do
          symbol "P"
          space
          char '['
   --       char '"'
   --       alta <- urlParse
          alta <- algo
          char ']'
          space
          char '['
   --       char '"'
   --       media <- urlParse
          media <- algo
          char ']'
          space
          char '['
  --        char '"'
  --        baja <- urlParse
          baja <- algo
          char ']'
          return $ D.P alta media baja

{-
urlParse :: Parser [Url]
urlParse = do
             sepBy ( aux2 ) (char ',') 

-- Para parsear paginas
aux2 :: Parser String
aux2 = many1(   alphanum
                 <|> char '.'
                 <|> char '/'
                 <|> char ':'
                 <|> char '_'
                 <|> char '-'
                 <|> char '\"' ) 

-}

-- Para parsear caracteres "raros" de las urls
auxU :: Parser Char
auxU =   alphanum 
         <|> char '.'
         <|> char '/'
         <|> char ':'
         <|> char '_'
         <|> char '-'
         <|> char '\\'
         <|> char '?'
         <|> char '='
         <|> alphanum
 


algo :: Parser [Url]
algo = (do
          char '\"'
          xs <- many auxU
          char '\"'
          ((do
              char ','
              x <- algo
              return ([xs]++x))
            <|> return ([xs]) )
        <|> 
          return [])
{-                         
num# -> # n | comment
n -> P [algo] [algo] [algo] n| Fuente i i n| Fondo i i n 
algo -> '"' string '"' (E | ','algo ) | E
comment -> alphanumsYspaces num#
-}

p1 :: [Config] -> Prior -> Parser ([Config],Prior)
p1 config (D.P a m b)  = (do
                            space
                            char '#' 
                            space
                            (do
                               space
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



--parse (p1 [] (D.P [] [] [])) "#P [] [\"http://www.lacapital.com\",\"http://www.rosario3.com\"] [] caca \n caca \n #Fondo 0 0 \n #Fuente 7 1 caca"

--parse (p1 [] (D.P [] [] [])) "#Fondo 4 0 \n#Fuente 3 1\n#P [http://www.clarin.com/,http://www.lacapital.com] [] []"


--print ( a(fst ((parse urlP  ( "P [\"hola][][]") !! 0) ) ) )

-- " #Fondo 4 0 \n#Fuente 3 1 \n #P [\"http://www.lacapital.com\"][][] "




parseNews :: News -> Parser News
parseNews (N a m b)= (do
                        space
                        char '#' 
                        space
                        (do
                           space
                           altanew <- altaNew
                           parseNews (N altanew m b)
                           <|> do
                                 space
                                 medianew <- mediaNew
                                 parseNews (N a medianew b)
                           <|> do
                                 space
                                 bajanew <- bajaNew
                                 parseNews (N a m bajanew) ) )
                            <|> do 
                                  space
                                  com1
                                  space
                                  parseNews (N a m b) 
                            <|> return (N a m b) 
                
altaNew :: Parser ([(String,Url)],Int)
altaNew = do
            symbol "NA"
            space
            alta <- parseoT
            return alta

mediaNew :: Parser ([(String,Url)],Int)
mediaNew = do
             symbol "NM"
             space
             media <- parseoT
             return media

bajaNew :: Parser ([(String,Url)],Int)
bajaNew = do
            symbol "NB"
            space
            baja <- parseoT
            return baja


parseoT  :: Parser ([(String,Url)],Int)
parseoT = do
            char '('
            t <- tuplas
            char ')'
            return t


tuplas :: Parser ([(String,Url)],Int)
tuplas = do
            char '['
            t <- tuplas'
            char ']'
            space
            char ','
            space
            n <- nat
            return (t,n)

tuplas' :: Parser [(String,Url)]
tuplas' = do
            char '('
            char '\"'
            space
            xs <- many auxN
            (string "\",\"" <|> string "\"\",\"")
            url <- many auxU
            char '\"'
            char ')'
            ((do
                 char ','
                 t2 <- tuplas'
                 return ( [(xs,url)]++t2 ) )
              <|> return ([(xs,url)]) )
          <|> return []

--Parsear los titulos de las noticias 
auxN :: Parser Char
auxN =   alphanum 
         <|> char '.'
         <|> char '/'
         <|> char ':'
         <|> char '_'
         <|> char '-'
         <|> char ','
         <|> char '$'
         <|> char '#'
         <|> char '@'
         <|> char '&'
         <|> char '\''      --apostrofe
         <|> char '+'
         <|> char '%'
         <|> char '“'
         <|> char '”'
         <|> char '!'
         <|> char '?'
         <|> char ';'
         <|> char '='
         <|> char '\147'
         <|> char '\148'
         <|> char '�'
         <|> latin1
         <|> upper
         <|> space'
         <|> (do 
                 char '\"'
                 (alphanum <|> upper <|> latin1 <|> char '.' <|> latin1 <|> ( do char '\"' ; char' ',' ))) 
         <|> (do char '\\'
                 char '\"')
         <|> (do 
                 char '“'
                 (alphanum <|> upper <|> latin1 <|> char '.' <|> latin1 <|> ( do char '“' ; char' ',' ))) 
         <|> (do char '\\'
                 char '“')
         <|> (do 
                 char '”'
                 (alphanum <|> upper <|> latin1 <|> char '.' <|> latin1 <|> ( do char '”' ; char' ',' ))) 
         <|> (do char '\\'
                 char '”')

--- Agrego caracteres (como las comillas a la izq o a der) por distintas representaciones y encoding en distintas paginas 


latin1 :: Parser Char
latin1 = (char '®' <|> char '¡' <|> char '¿' <|> char '°' <|> char 'º' <|> char 'Á' <|> char 'É' <|> char 'Í' <|> char 'Ó' <|> char 'Ú' <|> char 'Ü' <|> char 'Ñ' <|> char 'ñ' <|> char 'á' <|> char 'é' <|> char 'í' <|> char 'ó' <|> char 'ú' <|> char 'ü' )

--"("\"Lo hicimos una vez...\"","http://www.ole.com.ar/futbol-internacional/eliminatorias/hacer_0_1766823451.html")"

--"# NA ([(\"choque\",\"www.choque.com\")],1) \n# NM ([],0) \n # NB ([],0)"

{-  
 num# -> # n | comment
 n -> (NA | NM | NB ) parseoT
 parseoT -> "(" tuplas ")"
 tuplas -> "[" tuplas' "]" "," Int  
 tuplas' -> "(" "\"" auxN "\"" "," "\"" auxU "\"" ")" ( "," tupla' | E ) | E
 comment -> alphanumsYspaces num#
-}

-- parse (parseNews (N ([],0) ([],0) ([],0) )) "# NA ([(\"\"Le pido a la gobernadora Vidal que recorra los hospitales sin aviso\"\",\"http://www.clarin.com/cartas-al-pais/pido-gobernadora-vidal-recorra-hospitales-aviso_0_HkJtghb3g.html\"),(\"\"Le pido a la gobernadora Vidal que recorra los hospitales sin aviso\"\",\"http://www.clarin.com/cartas-al-pais/pido-gobernadora-vidal-recorra-hospitales-aviso_0_HkJtghb3g.html\")],2) \n# NM ([],0) \n# NB ([],0)"





