{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.IngredienteCardapio where

import Import
import Handler.Funcs (anyOriginIn)

postIngreCardapioAddR :: Handler TypedContent
postIngreCardapioAddR = do
    anyOriginIn
    produto <- requireJsonBody :: Handler IngredienteCardapio -- fazer parse para objeto 
    pid <- runDB $ insert produto -- 
    sendStatusJSON created201 (toJSON pid)
    
optionsIngreCardapioAddR :: Handler TypedContent
optionsIngreCardapioAddR  = do
   anyOriginIn
   sendStatusJSON ok200 (object [])

getIngreCardapioR :: CardapioId ->Handler TypedContent
getIngreCardapioR pid = do 
    anyOriginIn
    prod <-runDB $ selectList [ IngredienteCardapioCardapio ==. pid  ] []
    sendStatusJSON ok200 (toJSON  prod)
   
optionsIngreCardapioR :: CardapioId -> Handler TypedContent
optionsIngreCardapioR _ = do
   anyOriginIn
   sendStatusJSON ok200 (object [])
