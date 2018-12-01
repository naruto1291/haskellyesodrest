{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Mesa where

import Import
import Handler.Funcs (anyOriginIn)
getMesasR :: Handler TypedContent
getMesasR = do 
    addHeader "Access-Control-Allow-Origin" "*"
    prods <- runDB $ selectList [] [Asc MesaLogin]
    sendStatusJSON ok200 (toJSON  prods) 
    
postMesaAddR :: Handler TypedContent
postMesaAddR = do
    addHeader "Access-Control-Allow-Origin" "*"
    produto <- requireJsonBody :: Handler Mesa
    pid <- runDB $ insert produto
    sendStatusJSON created201 (toJSON pid)

getMesaEspecificaR ::Text  ->Text  -> Handler TypedContent
getMesaEspecificaR pid senha = do 
    addHeader "Access-Control-Allow-Origin" "*"
    prod <- runDB $ selectList [MesaLogin ==. pid,MesaSenha ==. senha] []
    sendStatusJSON ok200 (toJSON  prod)
    
optionsMesaupdateR :: MesaId -> Text -> Handler TypedContent
optionsMesaupdateR _ _ = do 
    anyOriginIn
    sendStatusJSON ok200 (object [])
    
patchMesaupdateR :: MesaId -> Text -> Handler TypedContent
patchMesaupdateR pid nome = do 
    _ <- runDB $ get404 pid
    anyOriginIn
    runDB $ update pid [MesaSenha =.  nome ] 
    sendStatusJSON noContent204 (object [])
    
optionsMesaupdateLogadoR :: MesaId -> Int -> Handler TypedContent
optionsMesaupdateLogadoR pid nome = do 
    anyOriginIn
    sendStatusJSON ok200 (object []) 
 
patchMesaupdateLogadoR :: MesaId -> Int -> Handler TypedContent
patchMesaupdateLogadoR pid nome = do 
    _ <- runDB $ get404 pid
    anyOriginIn
    runDB $ update pid [MesaLogado =.  nome ] 
    sendStatusJSON noContent204 (object [])
