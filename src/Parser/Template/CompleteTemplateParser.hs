module Parser.Template.CompleteTemplateParser (
    parseTemplate
) where

-- | Relevant imports
import Models.CompleteMustacheModel ( Template, Element (..) )
import Parser.Parser ( Parser, (<|>), some, many, carQuand, chaine, car )
import Data.Char ( isAlphaNum )
import Parser.Utils ( toutSauf, parseSpaces )
import Data.List (intercalate)

-- | Helper function to parse a name as a sequence of '.' separated strings
parseName :: Parser [String]
parseName = (:) <$> many (carQuand isAlphaNum) <*> many (car '.' >> many (carQuand isAlphaNum))

{- | Parses a Section

     A Section is defined as a title (a variable name)
     enclosed in double braces and preceded by a '#'
     then a template
     then a closing tag containing the variable name
     preceded by a '/'

     Example :
     {{ #name }}
     ... Template ...
     {{ /name }}
-}
parseSection :: Parser Element
parseSection = chaine "{{"             -- open section tag
                >> parseSpaces         -- eventual spacing
                >> chaine "#"          -- #
                >> parseName           -- section variable name
                >>= \n -> parseSpaces  -- eventual spacing
                >> chaine "}}"         -- close section tag
                >> ((car '\n' >> return ()) <|> return ()) -- eventual '\n'
                >> parseTemplate       -- section template
                >>= \template -> chaine "{{" -- open section tag
                >> parseSpaces         -- eventual spacing
                >> chaine ("/" ++ intercalate "." n)   -- section variable name again
                >> parseSpaces         -- eventual spacing
                >> chaine "}}"         -- close section tag
                >> ((car '\n' >> return ()) <|> return ()) -- eventual '\n'
                >> return (Section n template)

-- | Parses a placeholder - a variable enclosed in double curly braces
parsePlaceholder :: Parser Element
parsePlaceholder = chaine "{{"         -- open
                 >> parseSpaces        -- eventual spacing
                 >> parseName          -- variable name
                 >>= \n -> parseSpaces -- eventual spacing
                 >> chaine "}}"        -- close
                 >> return (Variable n)

-- | Parses simple text
parseText :: Parser Element
parseText = some (toutSauf "{}") >>= \s -> return $ Text s

-- | Parses multiple elements - either text, placeholders or sections
parseTemplate :: Parser Template
parseTemplate = many (parseSection <|> parsePlaceholder <|> parseText)
