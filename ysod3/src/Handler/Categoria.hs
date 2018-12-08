{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Categoria where

import Import

getCategoriaR :: Handler TypedContent
getCategoriaR = do 
   addHeader "Access-Control-Allow-Origin" "*"
   prods <- runDB $ selectList [] [Asc CategoriaNome]
   sendStatusJSON ok200 (toJSON prods )
   

postCategoriaAddR :: Handler TypedContent
postCategoriaAddR = do
    produto <- requireJsonBody :: Handler Categoria 
    pid <- runDB $ insert produto -- 
    sendStatusJSON created201 (toJSON pid)
    
    
getCategoriaporR :: CategoriaId  -> Handler TypedContent
getCategoriaporR pid = do 
    anyOriginIn
    prod <- runDB $ selectList [CategoriaId ==. pid] []
    sendStatusJSON ok200 (toJSON  prod)
