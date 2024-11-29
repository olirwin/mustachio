module Parser.Data.JsonParser (
  parseJSON
) where

import Parser.Parser (Parser, empty, (<|>), some, many, carQuand, car, chaine)
import Data.Char ( isDigit, isSpace )
import Models.JsonModel ( JSON (..) )
import Parser.Utils ( parseSpaces, (<++>), toutSauf, zeroOuPlusVirg)

--cut passeBlanc.hs
passeBlancs :: Parser a -> Parser a
passeBlancs p = p >>= \a -> many (carQuand isSpace) >> return a
--cutend
--cut parseNULL.hs
parseNULL :: Parser JSON
parseNULL = chaine "null" >> parseSpaces >> return JsonNull
--cutend
--cut parseBool.hs
parseBool :: Parser JSON
parseBool = JsonBool <$> passeBlancs (pTrue <|> pFalse)
            where
              pTrue  = chaine "true" >> return True
              pFalse = chaine "false" >> return False
--cutend 
--cut chaineOption.hs
chaineOption :: String -> Parser String
chaineOption s = chaine s <|> return ""
--cutend
--cut nombre.hs
nombre :: Parser String
nombre  = some (carQuand isDigit)
--cutend
--cut parseInt.hs
parseInt :: Parser JSON
parseInt = JsonInt <$> passeBlancs entier
  where
    entier = read <$> (chaineOption "-" <++> nombre)
--cutend
--cut parseFloat.hs
parseFloat :: Parser JSON
parseFloat = JsonFloat <$> passeBlancs flottant
  where
    flottant = read <$> (chaineOption "-" <++> nombre <++> chaine "."  <++> nombre)
--cutend

--cut chaineP.hs
-- chaineP :: Parser String
-- chaineP = car '"' >> many (toutSauf "\"") >>= \s ->  passeBlancs (car '"') >> return s
--cutend
--cut parseChaine.hs
parseChaine :: Parser JSON
parseChaine = JsonString <$> chaineEchP

-- parseChaineM :: Parser JSON
-- parseChaineM = chaineP >>= return . JsonString

-- parseChaineA :: Parser JSON
-- parseChaineA = JsonString <$> chaineP
--cut end
--cut parseJSON.hs
parseJSON :: Parser JSON
parseJSON = passeBlancs (parseNULL <|> parseBool <|> parseFloat <|> parseInt <|>
            parseChaine <|> parseArray <|> parseObject)
--cut end

--cut parseArray.hs
parseArray :: Parser JSON
parseArray = do
    passeBlancs (car '[')
    l <- zeroOuPlusVirg parseJSON
    passeBlancs (car ']')
    return (JsonArray l)

-- parseArrayM :: Parser JSON
-- parseArrayM = car '[' >> zeroOuPlusVirg parseJSON >>= \l -> car ']' >> return (JsonArray l)

-- parseArrayA :: Parser JSON
-- parseArrayA = (\_ l _ -> JsonArray l) <$> car '[' <*> zeroOuPlusVirg parseJSON <*> car ']'
--cut end
--cut clefValeur.hs
clefValeur :: Parser (String, JSON)
clefValeur = do
   str <- chaineEchP
   passeBlancs (car ':')
   js <- parseJSON
   return (str,js)
--cut end
--cut parseObject.hs
parseObject :: Parser JSON
parseObject = do
    passeBlancs (car '{')
    l <- zeroOuPlusVirg clefValeur
    passeBlancs (car '}')
    return (JsonObject l)

-- parseJsonObjectM :: Parser JSON
-- parseJsonObjectM = car '{' >> zeroOuPlusVirg clefValeur >>= \l -> car '}' >> return (JsonObject l)

-- parseJsonObjectA :: Parser JSON
-- parseJsonObjectA = (\_ l _ -> JsonObject l) <$> car '{' <*> zeroOuPlusVirg clefValeur <*> car '}'
--cut end

--cut echappement.hs
echappement :: (Char,Char) ->Parser Char
echappement (c,r) = car '\\' >> car c >> return r
--cut end

--cut listeEchappements.hs
listeEchappements :: [(Char,Char)]
listeEchappements = [('n', '\n'), ('t', '\t'), ('"', '"'), ('\\', '\\')]
--cut end

--cut echappement.hs
echappements :: Parser Char
echappements  = foldr (\p ech -> echappement p <|> ech) empty listeEchappements
--cut end

--cut chaineEchP.hs
chaineEchP :: Parser String
chaineEchP = car '"' >> many (echappements <|> toutSauf "\\\"") >>= \l -> passeBlancs (car '"') >> return l
--cut end
