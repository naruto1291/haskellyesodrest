{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where
import Handler.Funcs (anyOriginIn)
import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postHomeR :: Handler Value
postHomeR = do
    anyOriginIn
    userJ <- requireJsonBody :: Handler Mesa
    userM <- runDB . getBy . UniqueMesa $ mesaLogin userJ
    case userM of
        Nothing -> notAuthenticated
        Just (Entity uid user) ->
            if ((mesaSenha user) == (mesaSenha userJ)) then
                sendStatusJSON ok200 $ toJSON $ tokenize  uid else
                notAuthenticated
                
postHomeDoisR :: Handler Value
postHomeDoisR = do
    anyOriginIn
    userJ <- requireJsonBody :: Handler Garcom
    userM <- runDB . getBy . UniqueGarcom $ garcomNome userJ
    case userM of
        Nothing -> notAuthenticated
        Just (Entity uid user) ->
            if ((garcomSenha user) == (garcomSenha userJ)) then
                sendStatusJSON ok200 $ toJSON $ tokenize2  uid else
                notAuthenticated 