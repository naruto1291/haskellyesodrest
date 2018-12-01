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

getPedidoR :: Handler TypedContent
getPedidoR = do 
   anyOriginIn
   prods <- runDB $ selectList [] [Asc PedidoDatap]
   sendStatusJSON ok200 (toJSON  prods)
   
