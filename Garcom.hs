{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Garcom where

import Import

getGarcomEspecificaR ::Text  ->Text  -> Handler TypedContent
getGarcomEspecificaR pid senha = do 
    addHeader "Access-Control-Allow-Origin" "*"
    prod <- runDB $ selectList [GarcomNome ==. pid,GarcomSenha ==. senha] []
    sendStatusJSON ok200 (toJSON  prod)
    
postGarcomaddR :: Handler TypedContent
postGarcomaddR = do
    addHeader "Access-Control-Allow-Origin" "*"
    produto <- requireJsonBody :: Handler Garcom
    pid <- runDB $ insert produto
    sendStatusJSON created201 (toJSON pid)   

getGarcomR :: Handler TypedContent
getGarcomR = do 
    addHeader "Access-Control-Allow-Origin" "*"
    prods <- runDB $ selectList [] [Asc GarcomNome]
    sendStatusJSON ok200 (toJSON prods)