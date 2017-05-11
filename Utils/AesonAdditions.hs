-- Daniel Hanover, Aditya Vatsan, Final Project
module Utils.AesonAdditions where

import Data.Aeson
import Data.Void
import Data.HashMap.Strict
import Data.List as List
import Data.Aeson.Types (Parser)
import Data.Text hiding (length)
-- | New Data.Aeson Operator: Only permit values in `keys` in the JSON
(.:!!) :: Object -> [Text] -> Parser Value
(.:!!) o validKeys =
    if not (List.null (keys o \\ validKeys)) then
        fail "Invalid Keys Present"
    else
        return Null
