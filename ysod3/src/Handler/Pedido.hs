{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Pedido where

import Import
import Data.Text as T (pack)
import Handler.Funcs (anyOriginIn)

postPedidoAddR :: Handler TypedContent
postPedidoAddR = do
    anyOriginIn
    produto <- requireJsonBody :: Handler Pedido 
    utct <- liftIO getCurrentTime
    pid <- runDB $ insert $ produto {pedidoDatap = utctDay utct,pedidoHorap =  utct} 
    sendStatusJSON created201 (toJSON  pid)

getPedidoR :: Handler TypedContent
getPedidoR = do 
   anyOriginIn
   prods <- runDB $ selectList [] [Asc PedidoDatap]
   sendStatusJSON ok200 (toJSON  prods)
   
getPedidoPorData :: Day -> Handler TypedContent
getPedidoPorData d = do 
    anyOriginIn
    prods <- runDB $ selectList [ PedidoDatap ==. d ] [Asc PedidoId] 
    sendStatusJSON ok200 (toJSON prods)

getPedidoPorHora :: Text -> Handler TypedContent
getPedidoPorHora d = do 
    anyOriginIn
    utct <- liftIO getCurrentTime
    prods <- runDB $ selectList [PedidoHorap ==. utct ] [Asc PedidoId] 
    sendStatusJSON ok200 (toJSON prods)
    
getPedidoPorFechamento :: CompraId -> Handler TypedContent
getPedidoPorFechamento d = do 
    anyOriginIn
    prods <- runDB $ selectList [PedidoCompra ==. d ] [Asc PedidoId] 
    sendStatusJSON ok200 (toJSON prods)

optionsPedidoDescricaoR :: PedidoId -> Text -> Handler TypedContent
optionsPedidoDescricaoR pid nome = do 
    anyOriginIn
    sendStatusJSON ok200 (object [])

optionsPedidostatusR :: PedidoId -> Text -> Handler TypedContent
optionsPedidostatusR pid nome = do 
    anyOriginIn
    sendStatusJSON ok200 (object [])

optionsPedidoGarcomidR :: PedidoId -> GarcomId -> Handler TypedContent
optionsPedidoGarcomidR pid nome = do 
    anyOriginIn
    sendStatusJSON ok200 (object [])

optionsPedidoR :: Handler TypedContent
optionsPedidoR  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])

optionsPedidoAddR :: Handler TypedContent
optionsPedidoAddR  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])

optionsPedidoPorData :: Day -> Handler TypedContent
optionsPedidoPorData _  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])

optionsPedidoPorHora :: Text -> Handler TypedContent
optionsPedidoPorHora _  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])

optionsPedidoPorFechamento :: CompraId -> Handler TypedContent
optionsPedidoPorFechamento _  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])
   
deletePedidoDelR ::PedidoId-> Handler TypedContent 
deletePedidoDelR  pid =do
        anyOriginIn
        _ <- runDB $ get404 pid
        runDB $ delete pid
        sendStatusJSON noContent204 (object [])
        
optionsPedidoDelR ::  PedidoId -> Handler TypedContent
optionsPedidoDelR pid =do
    anyOriginIn
    sendStatusJSON ok200 (object [])