{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Compra where

import Import

import Handler.Funcs (anyOriginIn)

getCompraR :: Handler TypedContent
getCompraR = do 
   anyOriginIn
   prods <- runDB $ selectList [] [Asc CompraDataFecha] 
   sendStatusJSON ok200 (toJSON prods)

postCompraAddR :: Handler TypedContent
postCompraAddR = do
    anyOriginIn
    produto <- requireJsonBody :: Handler Compra
    utct <- liftIO getCurrentTime
    pid <- runDB $ insert $ produto {compraDataAbertura = utctDay utct, compraHoraAbertura = utct,compraHoraFecha = utct}
    prod <- runDB $ selectList [CompraId ==. pid ] [] 
    sendStatusJSON created201 (toJSON prod )
