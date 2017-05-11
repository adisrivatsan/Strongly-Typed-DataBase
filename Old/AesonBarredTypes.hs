b :: (FromJSON (Tree a), FromJSON a) => Object -> Parser (Tree a)
b o = 
    let s = HM.size (HM.difference o (HM.fromList [("son", ""), ("daughter", ""), ("age", "")])) in
    if s > 0 then fail "Invalid Keys" else
    do
        Branch <$> (o .: "son") <*> o .: "daughter" <*> o .: "age"

c :: FromJSON a => Object -> Parser (Tree a)
c o = 
    let s = HM.size (HM.difference o (HM.fromList [("age", "")])) in
    if s > 0 then fail "Invalid Keys" else
    do
        Leaf <$> o .: "age"

[
	o .:!! ["son", "daughter", "age"] *> (Branch <$> o .: "son" <*> o .: "daughter" <*> o .: "age"),
	o .:!! ["age"] *> (Leaf <$> o .: "age")
]

(.:) :: (FromJSON a) => Object -> Text -> Parser a
o .: key = case HM.lookup key o of
             Nothing -> fail ("key " ++ show key ++ " not present")
             Just v  -> parseJSON v

failOnOtherKeys :: [Text] -> Object 
(.:!!) :: FromJSON a => Object -> [String] -> Bool
(.:!!) o keys = do
    let s = HM.size (HM.difference o (HM.fromList (map (\x -> (x, "")) keys) ))
    if s > 0 then 
        fail "Invalid Keys Present" 
    else
        parseJSON o