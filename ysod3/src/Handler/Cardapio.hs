{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Categoria where

import Import
import Handler.Funcs (anyOriginIn)

getCardapioR :: Handler TypedContent
getCardapioR = do 
   anyOriginIn
   prods <- runDB $ selectList [] [Asc CardapioNome]
   sendStatusJSON ok200 (toJSON prods)
   

