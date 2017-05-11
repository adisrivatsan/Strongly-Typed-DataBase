data Val a = AA a | B Bool

data TypeStructureValue a = TS (HashMap Text (TypeStructureValue a)) (Value -> Bool)

data Person =
  Person { likesPizza :: Bool
           } deriving (Show,Generic)

instance FromJSON Person
instance ToJSON Person

isPerson :: Value -> Maybe Person
isPerson = testTemplate

genTypeStructureValue :: (Value -> Maybe a) -> TypeStructureValue a
genTypeStructureValue fun = TS Hash.empty (fun) (\t -> True)

genTypeStructureValueTrue :: TypeStructureValue (Val Bool)
genTypeStructureValueTrue = TS Hash.empty (\t -> Just (B True)) (\t -> True)

serverType :: TypeStructureValue (Val Bool)
serverType = TS (Hash.fromList
  [
  (Data.Text.pack("a"), genTypeStructureValueTrue),
  (Data.Text.pack("b"), genTypeStructureValueTrue),
  (Data.Text.pack("c"), genTypeStructureValueTrue)
  ]) (\t -> Just (B True)) (\t -> True)