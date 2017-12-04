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

data Assistido = Assistido {assistido::Bool} deriving Generic
instance ToJSON Assistido where
instance FromJSON Assistido where


postFilmesCadR :: Handler TypedContent
postFilmesCadR = do
    filmCad <- requireJsonBody :: Handler FilmesCad
    idFilmCad <- runDB $ insert filmCad
    sendStatusJSON created201 $ Retorno 0 (toJSON $ fromSqlKey idFilmCad)
    
getListarFilmesR :: CadastrosId -> Handler TypedContent
getListarFilmesR idCad =  do
    lista <- runDB $ selectList [FilmesCadIdCadastro ==. idCad] []
    lista' <- return $ fmap (\(Entity pkFilmesCad filmesCad) -> (fromSqlKey pkFilmesCad, filmesCad)) lista 
    filmesIds <- return $ fmap (filmesCadIdFilme . snd) lista'
    filmes <- sequence $ fmap (\fid -> runDB $ get404 fid) filmesIds 
    lstPKFilmesCad <- return $ fmap (id . fst) lista'
    sendStatusJSON ok200  (object ["resp" .= (object ["filmes" .= (toJSON filmes), "pks" .= (toJSON lstPKFilmesCad)]) ])

getListarFavoritosR :: CadastrosId -> Handler TypedContent
getListarFavoritosR idCad = do
    lista <- runDB $ selectList [FilmesCadIdCadastro ==. idCad, FilmesCadFavorito ==. True] []
    lista' <- return $ fmap (\(Entity _ filmesCad) -> filmesCad) lista 
    filmesIds <- return $ fmap filmesCadIdFilme lista'
    filmes <- sequence $ fmap (\fid -> runDB $ get404 fid) filmesIds 
    sendStatusJSON ok200 (object ["resp" .= (toJSON filmes)])

{-
getListarFavoritosR :: CadastrosId -> Handler TypedContent
getListarFavoritosR idCad = 
    -- let 
    --     concatlista :: [a] -> [a] -> [a]
    --     concatlista [] ys = ys
    --     concatlista xs [] = xs
    --     concatlista (x:xs) (y:ys) = x : y : concatlista xs y
    -- in
    do
    lista <- runDB $ selectList [FilmesCadIdCadastro ==. idCad, FilmesCadFavorito ==. True] []
    lista' <- return $ fmap (\(Entity pkFilmesCad filmesCad) -> (fromSqlKey pkFilmesCad, filmesCad)) lista 
    filmesIds <- return $ fmap (filmesCadIdFilme . snd) lista'
    filmes <- sequence $ fmap (\fid -> runDB $ get404 fid) filmesIds 
    lstPKFilmesCad <- return $ fmap (id . fst) lista'
    sendStatusJSON ok200  (object ["resp" .= (object ["filmes" .= (toJSON filmes), "pks" .= (toJSON lstPKFilmesCad)]) ])
-}
    
getListarAssistidosR:: CadastrosId -> Handler TypedContent
getListarAssistidosR idCad = do
    lista <- runDB $ selectList [FilmesCadIdCadastro ==. idCad, FilmesCadAssistido ==. True] []
    lista' <- return $ fmap (\(Entity _ filmesCad) -> filmesCad) lista 
    filmesIds <- return $ fmap filmesCadIdFilme lista'
    filmes <- sequence $ fmap (\fid -> runDB $ get404 fid) filmesIds 
    sendStatusJSON ok200 (object ["resp" .= (toJSON filmes)])
    
postAlterarFavoritosR :: FilmesCadId -> Handler Value
postAlterarFavoritosR idFilmesCad = do
    _ <- runDB $ get404 idFilmesCad
    newFav <- requireJsonBody :: Handler Favorito
    runDB $ update idFilmesCad [FilmesCadFavorito =. (favorito newFav)] 
    sendStatusJSON noContent204 (object ["resp" .= ("atualizado" ++ show (fromSqlKey idFilmesCad))])
    
postAlterarAssistidosR :: FilmesCadId -> Handler Value
postAlterarAssistidosR idFilmesCad = do
    _ <- runDB $ get404 idFilmesCad
    newAssistidos <- requireJsonBody :: Handler Assistido
    runDB $ update idFilmesCad [FilmesCadAssistido =. (assistido newAssistidos)] 
    sendStatusJSON noContent204 (object ["resp" .= ("atualizado" ++ show (fromSqlKey idFilmesCad))])

--arruma ramon 
-- deletarFilmesCadR :: FilmesCadId -> Handler Value
-- deletarFilmesCadR idFilmesCad = do
--     _ <- runDB $ get404 idFilmesCad
--     runDB $ delete idFilmesCad
--     sendStatusJSON noContent204 (object ["resp" .= ("deletado" ++ show (fromSqlKey idFilmesCad))])