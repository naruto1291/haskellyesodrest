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