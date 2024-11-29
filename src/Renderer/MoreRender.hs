module Renderer.MoreRender (
    instTemplate
) where

-- | Relevant imports
import Models.JsonModel ( JSON (..) )
import Models.CompleteMustacheModel ( Template, Element (..) )
import Control.Applicative ( liftA2 )
import Renderer.Utils ( dataToString )

-- | Gets the value from a key name inside a JSON Object
getValue :: JSON -> [String] -> Maybe JSON
getValue (JsonObject obj) [x]    = lookup x obj
getValue (JsonObject obj) (x:xs) = case lookup x obj of
    Nothing   -> Nothing
    Just json -> getValue json xs
getValue _ _                     = Nothing

-- | Converts an Element to a String with help from JSON Object env
instElement :: JSON -> Element -> Maybe String
instElement json (Variable x)      = dataToString <$> getValue json x
instElement _    (Text t)          = Just t
instElement json (Section n templ) = case getValue json n of
    Just (JsonArray l) -> concat <$> mapM (`instTemplate` templ) l
    Just (JsonBool b)  -> if b then instTemplate json templ else Just ""
    _                  -> Nothing

-- | Converts a template to a String with help from JSON Object env
instTemplate :: JSON -> Template -> Maybe String
instTemplate json = foldr (liftA2 (++) . instElement json) (Just "")
