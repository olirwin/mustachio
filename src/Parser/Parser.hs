module Parser.Parser (Parser, runParser, evalParser, empty, (<|>), some, many, carQuand, unCaractereQuelconque, car, chaine) where
import Control.Applicative
type Resultat a = Maybe (a,String)

newtype Parser a = MkParser {runParser :: String -> Resultat a}

evalParser :: Parser a -> String -> Maybe a
evalParser p s = fst <$> runParser p s

instance Functor Parser  where
  -- On construit un nouveau Parser Ã  partir de p
  fmap f p = MkParser $ \s0 ->
                        -- on exÃ©cute p sur l'entrÃ©e s0
                        case runParser p s0 of
                          -- si p rÃ©ussit, alors le nouveau Parser renvoie le
                          -- rÃ©sultat de p auquel on a appliquÃ©e f
                          Just (a,s1) -> Just(f a, s1)
                          -- si p Ã©choue, le nouveau Parser Ã©choue
                          Nothing    -> Nothing

instance Applicative Parser where
  -- On construit un Parser qui ne regarde pas son entrÃ©e et qui renvoie
  -- directement a.
  pure a = MkParser $ \s -> Just (a, s)
  -- Ã‰tant donnÃ© un parser p1 de type Parser (a -> b) et un parser p2 de type
  -- Parser a, on souhaite construire un nouveau parser de type Parser b qui :
  -- 1. exÃ©cute p1, puis exÃ©cute p2, et
  -- 2. renvoie l'application de la fonction calculÃ©e par p1 sur la valeur calculÃ©e par p2
  -- 4. Ã©choue si p1 ou p2 Ã©choue
  p1 <*> p2 = MkParser $ \s0 ->
                         -- on commence par Ã©valuer p1 sur l'entrÃ©e l0
                         case runParser p1 s0 of
                           -- si p1 rÃ©ussit, on Ã©value p2 sur ce qu'il reste de
                           -- la chaÃ®ne Ã  analyser
                           Just (f, s1) -> case runParser p2 s1 of
                                   -- si p2 rÃ©ussit, on renvoie f (rÃ©sultat de p1) Ã  b (rÃ©sultat de p2)
                                   -- et la chaÃ®ne restante est celle qui reste aprÃ¨s l'exÃ©cution de p2.
                                             Just (b, s2) -> Just (f b, s2)
                                             -- si p2 Ã©choue, le nouveau parser Ã©choue
                                             Nothing      -> Nothing
                           -- si p1 Ã©choue, le nouveau parser Ã©choue
                           Nothing  -> Nothing

instance Alternative Parser where
  -- On construit un Parser qui Ã©choue systÃ©matiquement sans regarder son
  -- entrÃ©e.
  empty = MkParser $ const Nothing

  -- On construit l'opÃ©rateur d'alternative qui exÃ©cute p1 et en cas d'Ã©chec exÃ©cute p2.
  p1 <|> p2 = MkParser $ \s0 ->
                         -- on exÃ©cute p1 sur l'entrÃ©e s0
                         case runParser p1 s0 of
                           -- en cas d'Ã©chec, on exÃ©cute p2 sur s0
                           Nothing -> runParser p2 s0
                           -- en cas de succÃ¨s, on renvoie le rÃ©sultat de p1
                           r       -> r
  -- NB : les dÃ©finitions de some et many sont donnÃ©es par les dÃ©finitions par
  --      dÃ©faut de la classe Alternative

instance Monad Parser where
  -- On construit un Parser qui Ã©tant donnÃ© un Parser p1, l'exÃ©cute, puis passe
  -- son rÃ©sultat Ã  une fonction fp2 qui produit un nouveau Parser.
  p1 >>= fp2 = MkParser $ \s0 ->
                          -- on exÃ©cute p1 sur l'entrÃ©e l0
                          case runParser p1 s0 of
                            -- en cas de succÃ¨s, si a est le rÃ©sultat de p1 et
                            -- si ce qu'il reste Ã  analyser est l1, alors on
                            -- exÃ©cute (p2 a) sur l1.
                            Just (a, s1) -> runParser (fp2 a) s1
                            -- en cas d'Ã©chec, on propage l'Ã©chec de p1
                            Nothing      -> Nothing

-- Nous pouvons maintenant dÃ©finir les premiers opÃ©rateurs pour construire les Parsers.

carQuand :: (Char -> Bool) -> Parser Char
carQuand p = MkParser $ \s ->
                        case s of
                          []     -> Nothing
                          (c:cs) -> if p c then Just (c,cs) else Nothing

unCaractereQuelconque :: Parser Char
unCaractereQuelconque = carQuand (const True)

car :: Char -> Parser Char
car c = carQuand (==c)

chaine :: String -> Parser String
chaine = foldr
  (\c ch -> car c >> ch >>= return . (c:))
  (return "")
