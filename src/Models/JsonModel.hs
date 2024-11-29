module Models.JsonModel (
    JSON (..)
) where

data JSON = JsonNull                     -- Valeur null
          | JsonBool Bool                -- Booléens
          | JsonInt Int                  -- Entiers
          | JsonFloat Float              -- Flottants
          | JsonString String            -- Chaînes de caractères
          | JsonArray  [JSON]            -- Liste
          | JsonObject  [(String, JSON)] -- Objets
          deriving Show
