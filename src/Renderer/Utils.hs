module Renderer.Utils (
    dataToString
) where
import Models.JsonModel ( JSON(..) )

-- | Converts a JSON value to a String
dataToString :: JSON -> String
dataToString (JsonString s) = s
dataToString (JsonBool b)   = show b
dataToString (JsonInt n)    = show n
dataToString (JsonFloat f)  = show f
dataToString _              = ""
