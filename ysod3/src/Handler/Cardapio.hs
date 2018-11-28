{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Cardapio where

import Import
import Handler.Funcs (anyOriginIn)

getCardapioR :: Handler TypedContent
getCardapioR = do 
   anyOriginIn
   prods <- runDB $ selectList [] [Asc CardapioNome]
   sendStatusJSON ok200 (toJSON prods)
   
postCardapioAddR :: Handler TypedContent
postCardapioAddR = do
    anyOriginIn
    produto <- requireJsonBody :: Handler Cardapio 
    pid <- runDB $ insert produto -- 
    sendStatusJSON created201 (toJSON pid)

