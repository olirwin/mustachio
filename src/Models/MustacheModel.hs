module Models.MustacheModel (
    Template, Element (..)
) where

type Template = [Element]

data Element = Text String
             | Variable String
             deriving (Show, Eq)
