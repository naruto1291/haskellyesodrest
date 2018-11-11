{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Mesa where

import Import

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