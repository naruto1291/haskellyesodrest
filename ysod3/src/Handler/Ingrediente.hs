{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Ingrediente where

import Import
import Data.Text as T (pack)
import Prelude as R (read)
import Handler.Funcs (anyOriginIn)

getIngredientesR :: Handler TypedContent
getIngredientesR = do 
    addHeader "Access-Control-Allow-Origin" "*"
    prods <- runDB $ selectList [] [Asc IngredienteId]
    sendStatusJSON ok200 (toJSON  prods)
   
postIngredienteAddR :: Handler TypedContent
postIngredienteAddR = do
    addHeader "Access-Control-Allow-Origin" "*"
    produto <- requireJsonBody :: Handler Ingrediente -- fazer parse para objeto 
    pid <- runDB $ insert produto -- 
    sendStatusJSON created201 (toJSON  pid)
    
optionsIngredienteAddR :: Handler TypedContent
optionsIngredienteAddR = do
                         anyOriginIn
                         sendStatusJSON ok200 (object [])

optionsIngredientesR :: Handler TypedContent
optionsIngredientesR = do
                        anyOriginIn
                        sendStatusJSON ok200 (object [])
