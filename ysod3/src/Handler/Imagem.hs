{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Imagem where

import Import
import Handler.Funcs (anyOriginIn)
-- import Prelude as P (writeFile, readFile)
import Database.Persist.Sql (fromSqlKey)

postImagemR :: CardapioId ->  Handler TypedContent 
postImagemR pid = do 
--  prods <- runDB $ selectList [CardapioId=. pid] []
    anyOriginIn
    imagem <- requireJsonBody :: Handler String
    logado <- runDB $ selectFirst [CardapioId ==. pid ] []
    case logado of
        Just x -> do
           liftIO $ writeFile ("dynamic" </> show (fromSqlKey pid)) $ encodeUtf8 $ pack  imagem
           sendStatusJSON noContent204 (object [])
        Nothing -> do 
            sendStatusJSON noContent204 (object [])--object ["resp" =. "erro"]
            
optionsImagemR :: CardapioId -> Handler TypedContent
optionsImagemR _  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])