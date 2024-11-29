module Models.CompleteMustacheModel (
    Template, Element (..)
) where

type Template = [Element]

data Element = Text String
             | Variable [String]
             | Section {
                name     :: [String],
                elements :: Template
             }
             deriving (Show, Eq)
