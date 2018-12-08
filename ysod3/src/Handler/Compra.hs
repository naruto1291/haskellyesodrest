{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Compra where

import Import

import Import
import Data.Text as T (pack)
import Prelude as R (read)
import Handler.Funcs (anyOriginIn)

postFechamentoAddR :: Handler TypedContent
postFechamentoAddR = do
    anyOriginIn
    produto <- requireJsonBody :: Handler Compra -- fazer parse para objeto 
    utct <- liftIO getCurrentTime
    pid <- runDB $ insert $ produto {compraDataAbertura = utctDay utct, compraHoraAbertura = utct,compraHoraFecha = utct} -- 
    prod <- runDB $ selectList [CompraId ==. pid ] [] 
    sendStatusJSON created201 (toJSON prod )

getFechamentoR :: Handler TypedContent
getFechamentoR = do 
   anyOriginIn
   prods <- runDB $ selectList [] [Asc CompraDataFecha] 
   sendStatusJSON ok200 (toJSON prods)

getFechamentoUmR :: CompraId -> Handler TypedContent
getFechamentoUmR pid = do 
    anyOriginIn
    prod <- runDB $ get404 pid
    sendStatusJSON ok200 (toJSON prod)

getFechamentoPorData :: Day -> Handler TypedContent
getFechamentoPorData d = do 
    anyOriginIn
    prods <- runDB $ selectList [CompraDataAbertura ==. d ] [Asc CompraDataFecha] 
    sendStatusJSON ok200 (toJSON prods)
    
getFechamentoPorHoraFechamentoR :: Text -> Day -> Handler TypedContent
getFechamentoPorHoraFechamentoR hora dia = do 
    anyOriginIn
    prods <- runDB $ selectList [CompraDataFecha ==. dia ] [Asc CompraDataFecha] --CompraHoraFecha >=. hora ,
    sendStatusJSON ok200 (toJSON prods)
    

patchFechamentodataUpdateR :: CompraId -> Day -> Handler TypedContent
patchFechamentodataUpdateR pid nome = do 
    _ <- runDB $ get404 pid
    anyOriginIn
    runDB $ update pid [CompraDataFecha =. nome]
    sendStatusJSON noContent204 (object [])

patchFechamentohoraUpdateR :: CompraId -> Text -> Handler TypedContent
patchFechamentohoraUpdateR pid nome = do 
    _ <- runDB $ get404 pid
    anyOriginIn
    utct <- liftIO getCurrentTime
    runDB $ update pid [CompraHoraFecha =. utct]
    sendStatusJSON noContent204 (object [])

patchFechamentovalorUpdateR :: CompraId -> Text -> Handler TypedContent
patchFechamentovalorUpdateR pid nome = do 
    _ <- runDB $ get404 pid
    anyOriginIn
    runDB $ update pid [CompraValor =. (  R.read ( unpack nome) ::Double) ] --readMaybe 
    sendStatusJSON noContent204 (object [])
    
    
    
optionsFechamentodataUpdateR :: CompraId -> Day -> Handler TypedContent
optionsFechamentodataUpdateR pid nome = do 
    anyOriginIn
    sendStatusJSON ok200 (object [])
optionsFechamentohoraUpdateR :: CompraId -> Text -> Handler TypedContent
optionsFechamentohoraUpdateR pid nome = do 
    anyOriginIn
    sendStatusJSON ok200 (object [])

optionsFechamentovalorUpdateR :: CompraId -> Text -> Handler TypedContent
optionsFechamentovalorUpdateR pid nome = do 
    anyOriginIn
    sendStatusJSON ok200 (object [])


optionsFechamentoPorHoraFechamentoR :: Text -> Day -> Handler TypedContent
optionsFechamentoPorHoraFechamentoR _ _  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])

optionsFechamentoPorData :: Day -> Handler TypedContent
optionsFechamentoPorData _  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])

optionsFechamentoAddR :: Handler TypedContent
optionsFechamentoAddR  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])

optionsFechamentoR :: Handler TypedContent
optionsFechamentoR  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])

optionsFechamentoUmR :: CompraId -> Handler TypedContent
optionsFechamentoUmR _  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])
