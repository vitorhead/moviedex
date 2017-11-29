{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.FilmesCad where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Utils

data Favorito = Favorito {favorito::Bool} deriving Generic
instance ToJSON Favorito where
instance FromJSON Favorito where

postFilmesCadR :: Handler TypedContent
postFilmesCadR = do
    filmCad <- requireJsonBody :: Handler FilmesCad
    idFilmCad <- runDB $ insert filmCad
    sendStatusJSON created201 $ Retorno 0 (toJSON $ fromSqlKey idFilmCad)
    
getListarFilmesR :: CadastrosId -> Handler TypedContent
getListarFilmesR idCad =  do
    lista <- runDB $ selectList [FilmesCadIdCadastro ==. idCad] []
    lista' <- return $ fmap (\(Entity _ filmesCad) -> filmesCad) lista 
    filmesIds <- return $ fmap filmesCadIdFilme lista' --extrair todos os IDs 
    filmes <- sequence $ fmap (\fid -> runDB $ get404 fid) filmesIds --o sequence vai fazer uma ação pra todos
    sendStatusJSON ok200 (object ["resp" .= (toJSON filmes)])
    
getListarFavoritosR :: CadastrosId -> Handler TypedContent
getListarFavoritosR idCad = do
    lista <- runDB $ selectList [FilmesCadIdCadastro ==. idCad, FilmesCadFavorito ==. True] []
    lista' <- return $ fmap (\(Entity _ filmesCad) -> filmesCad) lista 
    filmesIds <- return $ fmap filmesCadIdFilme lista'
    filmes <- sequence $ fmap (\fid -> runDB $ get404 fid) filmesIds 
    sendStatusJSON ok200 (object ["resp" .= (toJSON filmes)])
    
getListarAssistidosR:: CadastrosId -> Handler TypedContent
getListarAssistidosR idCad = do
    lista <- runDB $ selectList [FilmesCadIdCadastro ==. idCad, FilmesCadAssistido ==. True] []
    lista' <- return $ fmap (\(Entity _ filmesCad) -> filmesCad) lista 
    filmesIds <- return $ fmap filmesCadIdFilme lista'
    filmes <- sequence $ fmap (\fid -> runDB $ get404 fid) filmesIds 
    sendStatusJSON ok200 (object ["resp" .= (toJSON filmes)])
    
patchAlterarFavoritosR :: FilmesCadId -> Handler Value
patchAlterarFavoritosR idFilmesCad = do
    _ <- runDB $ get404 idFilmesCad
    newFav <- requireJsonBody :: Handler Favorito
    runDB $ update idFilmesCad [FilmesCadFavorito =. (favorito newFav)] 
    sendStatusJSON noContent204 (object ["resp" .= ("atualizado" ++ show (fromSqlKey idFilmesCad))])

