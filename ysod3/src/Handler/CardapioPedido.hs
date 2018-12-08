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