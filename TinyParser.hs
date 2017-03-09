module TinyParser where

import Parsing
import Dataparalelo




--fond :: Parser Config
fond = do
         symbol "Fondo"
         space
         c <- nat
         space
         i <- nat
         return $ Fondo c i
