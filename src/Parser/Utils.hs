module Parser.Utils (
    parseSpaces, (<++>), toutSauf, unOuPlusVirg, zeroOuPlusVirg, virgule
) where

import Parser.Parser (Parser, (<|>), many, car, carQuand)
import Data.Char (isSpace)

parseSpaces :: Parser ()
parseSpaces = many (carQuand isSpace) >> return ()

(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
p1 <++> p2 = (++) <$> p1 <*> p2

toutSauf :: [Char] -> Parser Char
toutSauf cs = carQuand (`notElem` cs)

virgule :: Parser ()
virgule = car ',' >> parseSpaces >> return ()

unOuPlusVirg :: Parser a -> Parser [a]
unOuPlusVirg p = (:) <$> p <*> many (virgule >> p)

zeroOuPlusVirg :: Parser a -> Parser [a]
zeroOuPlusVirg p = unOuPlusVirg p  <|> return []
