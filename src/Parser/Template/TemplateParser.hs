module Parser.Template.TemplateParser (
    parseTemplate
) where

-- | Relevant imports 
import Models.MustacheModel ( Template, Element (..) )
import Parser.Parser ( Parser, (<|>), some, many, chaine )
import Parser.Utils ( toutSauf, parseSpaces )

-- | Parses a placeholder - a variable enclosed in double curly braces
parsePlaceholder :: Parser Element
parsePlaceholder = chaine "{{" -- open
                 >> parseSpaces -- eventual spacing
                 >> many (toutSauf "/{} ") -- variable name
                 >>= \s -> parseSpaces -- eventual spacing
                 >> chaine "}}" -- close
                 >> return (Variable s)

-- | Parses simple text
parseText :: Parser Element
parseText = some (toutSauf "{}") >>= \s -> return $ Text s 

-- | Parses multiple elements - either text or placeholders
parseTemplate :: Parser Template
parseTemplate = many (parsePlaceholder <|> parseText)
