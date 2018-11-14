{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Ingrediente where

import Import

getIngredientesR :: Handler TypedContent
getIngredientesR = do 
    addHeader "Access-Control-Allow-Origin" "*"
    prods <- runDB $ selectList [] [Asc IngredienteId]
    sendStatusJSON ok200 (toJSON  prods)
   
