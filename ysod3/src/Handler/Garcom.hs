{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Garcom where

import Import

getGarcomEspecificaR ::Text  ->Text  -> Handler TypedContent
getGarcomEspecificaR pid senha = do 
    addHeader "Access-Control-Allow-Origin" "*"
    prod <- runDB $ selectList [GarcomNome ==. pid,GarcomSenha ==. senha] []
    sendStatusJSON ok200 (toJSON  prod)