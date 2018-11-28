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
