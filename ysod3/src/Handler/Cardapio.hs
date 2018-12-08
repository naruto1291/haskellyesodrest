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
    pid <- runDB $ insert produto
    sendStatusJSON created201 (toJSON pid)

patchCardapioupdateR :: CardapioId -> Text -> Handler TypedContent
patchCardapioupdateR pid nome = do 
    _ <- runDB $ get404 pid
    anyOriginIn
    runDB $ update pid [CardapioDisponivel =. (  R.read ( unpack nome) :: Bool) ] --readMaybe 
    sendStatusJSON noContent204 (object [])
    
optionsCardapioupdateR :: CardapioId -> Text -> Handler TypedContent
optionsCardapioupdateR pid nome = do 
    anyOriginIn
    sendStatusJSON ok200 (object [])
    
optionsCardapioR :: Handler TypedContent
optionsCardapioR  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])

optionsCardapioAddR :: Handler TypedContent
optionsCardapioAddR  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])
