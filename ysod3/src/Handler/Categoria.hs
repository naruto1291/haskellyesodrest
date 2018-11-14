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
   
