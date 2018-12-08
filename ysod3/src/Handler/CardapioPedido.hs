{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE DeriveGeneric #-} -------
module Handler.CardapioPedido where

import Import
import Handler.Funcs

postCardapioPedidoAddR :: Handler TypedContent
postCardapioPedidoAddR = do
    anyOriginIn
    produto <- requireJsonBody :: Handler CardapioPedido -- fazer parse para objeto 
    pid <- runDB $ insert produto -- 
    sendStatusJSON created201 (toJSON pid)

optionsCardapioPedidoAddR :: Handler TypedContent
optionsCardapioPedidoAddR  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])
   
getCardapioPedidoR :: Handler TypedContent
getCardapioPedidoR = do 
   anyOriginIn
   prods <- runDB $ selectList [] [Asc IngredienteCardapioId] 
   sendStatusJSON ok200 (toJSON prods)
   
optionsCardapioPedidoR :: Handler TypedContent
optionsCardapioPedidoR  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])
   
getCardapioPedidoJoinR :: PedidoId   -> Handler TypedContent        
getCardapioPedidoJoinR pid = do 
    anyOriginIn
    vendas <- runDB $ selectList [ CardapioPedidoPedido ==. pid] []
    proids <- return $ fmap( cardapioPedidoCardapio . entityVal) vendas
    clientes <- runDB $ selectList[CardapioId <-. proids ][]
    sendStatusJSON ok200 (toJSON  clientes)   

optionsCardapioPedidoJoinR :: PedidoId   -> Handler TypedContent
optionsCardapioPedidoJoinR _  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])
   
getCardapioPedidoDuploJoinR :: PedidoId   -> Handler TypedContent        
getCardapioPedidoDuploJoinR pid = do 
    anyOriginIn
    vendas <- runDB $ selectList [ CardapioPedidoPedido ==. pid] []
    proids <- return $ fmap( cardapioPedidoCardapio . entityVal) vendas
    clientes <- runDB $ selectList[CardapioId <-. proids ][]
    sendStatusJSON ok200  (object ["pedidoCardapio" .= vendas ,"cardapio" .= (clientes),"pedidoid".=pid  ])

optionsCardapioPedidoDuploJoinR :: PedidoId   -> Handler TypedContent   
optionsCardapioPedidoDuploJoinR _  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])
