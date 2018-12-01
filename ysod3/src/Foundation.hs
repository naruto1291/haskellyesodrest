{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
 
module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)
import qualified Web.JWT    as JWT
import qualified Data.Map   as M
import Data.Aeson (Result(..), fromJSON)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static
    , appConnPool    :: ConnectionPool
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

segredo :: Text
segredo = "segredoAqui"

chave :: Text
chave = "chaveDoJsonAqui"

untokenize :: Text -> Maybe Value
untokenize t = (\x -> chave `M.lookup` JWT.unregisteredClaims (JWT.claims x))
    =<< JWT.decodeAndVerifySignature (JWT.secret segredo) t

tokenize :: MesaId -> Text
tokenize u = JWT.encodeSigned JWT.HS256 (JWT.secret segredo)
    def {JWT.unregisteredClaims = M.fromList [(chave, toJSON u)]}

tokenize2 :: GarcomId -> Text
tokenize2 u = JWT.encodeSigned JWT.HS256 (JWT.secret segredo)
    def {JWT.unregisteredClaims = M.fromList [(chave, toJSON u)]}
    
instance Yesod App where
    makeLogger = return . appLogger

--    isAuthorized HomeR _ = return Authorized
--    isAuthorized HomeDoisR _ = return Authorized
    isAuthorized _ False = return Authorized
    isAuthorized (GarcomEspecificaR _ _) False = return Authorized
    
    isAuthorized (MesaEspecificaR _ _) _ = return Authorized
    isAuthorized (GarcomEspecificaR _ _) True = maybeAuthId >>= return . \case
        Nothing -> Unauthorized "Precisa logar"
        _       -> Authorized
    isAuthorized _     _ = maybeAuthId >>= return . \case
        Nothing -> Unauthorized "Precisa logar"
        _       -> Authorized

instance YesodAuth App where
    type AuthId App = MesaId

    maybeAuthId = return . join . fmap
        (fmap ((\(Success x) -> x) . fromJSON) . untokenize . decodeUtf8)
        =<< lookupHeader "Authorization"

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager  