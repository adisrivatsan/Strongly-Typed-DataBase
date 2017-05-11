-- validateHelper :: RecursiveValidator -> Value -> Bool
-- validateHelper (V tmap _ _) val [x]     = case Hash.lookup x tmap of
--    Nothing     -> True
--    Just submap -> subHelper submap x val
-- validateHelper (V tmap _ _) val (x:xs)  = case Hash.lookup x tmap of
--    Nothing     -> True
--    Just submap -> validateHelper submap val xs
-- validateHelper tmap val _              = True --Rule not found, default to true


-- app2 :: Text -> WS.ClientApp ()
-- app2 line conn = do
--     Prelude.putStrLn "Connected!"
--
--     unless (T.null line) $ WS.sendTextData conn line
--
--
--     _ <- forkIO $ forever $ do
--       msg <- WS.receiveData conn
--       t <- example msg
--       liftIO $ T.putStrLn t
--
--
--     WS.sendClose conn ("Bye!" :: Text)
--
--
-- testapp2 :: IO ()
-- testapp2 = do
--   withSocketsDo $ WS.runClient "localhost" 8080 "/" (app2 (T.pack("{\"path\": \"a\"}")))




-- action :: Text -> IO String
-- action t = do
--   return (show t)


-- listen :: Text -> (Text -> IO Text) -> IO Text
-- listen t fun = fun t
--
-- -- example :: Text -> IO Text
-- -- example msg = listen msg (\x -> return (T.pack ("a")))
--
-- listenToP :: [Text] -> (Text -> Maybe (Text -> IO a)) -> IO a
-- listenToP path filt = undefined

-- checkPathExec :: [Text] -> (Value -> Maybe Value) -> Maybe Value -> Res
-- checkPathExec a b c = execPath (checkPathMaybe a b c)

-- execPath :: Maybe Value -> Res
-- execPath val = case val of
--   Nothing -> T $ return $ Left "type invalid"
--   Just a -> T $ return $ Right (T.pack (show a))
--

  -- case T.breakOn (T.pack ":") msg of
  -- (x,y) -> if userPath == (T.splitOn (T.pack "/") x) then func (T.drop 1 y)  -- (Prelude.foldr (\x acc -> T.append (T.append x (T.pack ":")) acc) (T.pack "") xs)
  --   else T $ return $ Left "error"
  -- (x:xs) -> T $ return $ Left "error"
  -- [] -> T $ return $ Left "error"

  -- rest :: Text -> Text
  -- rest t = case (show (T.strip t)) of
  --   (x:xs) -> T.strip $ T.pack xs
  --   [] -> T.pack ""

  -- pathB :: Value -> Maybe Value
  -- pathB t = case (fromJSON t :: Result Person) of
  --   Error _ -> Nothing
  --   Success a -> Just (Data.Aeson.String (T.pack "yay"))
  -- pathB t = case decode t :: Maybe Person of
  --   Nothing -> T $ return $ Left "error"
  --   Just a -> T $ return $ Right $ T.pack (show (a))

  -- pathXY :: Value -> Maybe Value
  -- pathXY t = Just t

  -- filterPaths :: [Res] -> IO Text
  -- filterPaths ((T x):xs) = do
  --   a <- x
  --   case a of
  --     Left b -> filterPaths xs
  --     Right c -> return c
  -- filterPaths [] = return (T.pack "type mis-match")

  -- testPathB = case pathB (T.pack "{likesPizza:true}") of
  --   T a -> do
  --     b <- a
  --     case b of
  --       Right x -> T.putStrLn x
  --       Left s -> T.putStrLn (T.pack s)
