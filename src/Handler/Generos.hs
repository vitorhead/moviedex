{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Generos where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Utils

getListarGeneroCadR :: CadastrosId -> Handler TypedContent
getListarGeneroCadR cid = do
    generos <- runDB $ selectList [GenerosIdCadastro <=. cid] [] 
    sendStatusJSON ok200 (object ["resp" .= toJSON generos])
    
deleteDeletarGeneroR :: GenerosId -> Handler Value
deleteDeletarGeneroR gid = do
    _ <- runDB $ get404 gid 
    runDB $ delete gid
    sendStatusJSON noContent204 (object ["resp" .= ("deletado" ++ show (fromSqlKey gid))])
    
putAlterarGeneroR :: GenerosId -> Handler Value
putAlterarGeneroR gid = do
    _ <- runDB $ get404 gid
    novoGenero <- requireJsonBody :: Handler Generos
    runDB $ replace gid novoGenero
    sendStatusJSON noContent204 (object ["resp" .= ("atualizado" ++ show (fromSqlKey gid))])